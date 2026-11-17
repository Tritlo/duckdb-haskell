#!/usr/bin/env python3
"""Bundle the demo extension built by build-linux.sh on Ubuntu 24.04."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tarfile


def run(*args: str) -> str:
    """Run a metadata tool and require success."""
    return subprocess.check_output(args, text=True)


def digest(path: Path) -> str:
    """Hash an artifact after its final modification."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def require(condition: bool, message: str) -> None:
    """Reject an unsupported artifact."""
    if not condition:
        raise SystemExit(message)


def metadata(path: Path) -> dict[str, list[str]]:
    """Read architecture, dependencies, and required symbol versions."""
    header = run('readelf', '-h', str(path))
    require('ELF64' in header and 'Advanced Micro Devices X86-64' in header,
            f'not a Linux x86_64 ELF object: {path}')
    dynamic = run('readelf', '-d', str(path))
    needed = re.findall(r'\(NEEDED\).*\[(.*?)\]', dynamic)
    require(not any('duckdb' in name and not name.startswith('libHS') for name in needed),
            f'unexpected native DuckDB import: {path}')
    versions = run('readelf', '--version-info', str(path))
    versions = versions.split('Version needs section', 1)[-1] if 'Version needs section' in versions else ''
    names = sorted(set(re.findall(r'Name: (\S+)', versions)))
    for name in names:
        if name.startswith('GLIBC_'):
            require(bool(re.fullmatch(r'GLIBC_[0-9.]+', name)), f'unsupported glibc requirement: {name}')
            require(tuple(map(int, name[6:].split('.'))) <= (2, 39), f'glibc newer than Ubuntu 24.04: {path}: {name}')
    return {'needed': needed, 'symbol_versions': names,
            'soname': re.findall(r'\(SONAME\).*\[(.*?)\]', dynamic)}


def package_fields(store: Path) -> list[dict[str, str]]:
    """Read the package metadata for the exact extension build plan."""
    args = ['ghc-pkg', 'dump', '--global']
    for database in sorted(store.glob('*/package.db')):
        args.append(f'--package-db={database}')
    args.append('--package-db=/work/build/packagedb/ghc-9.14.1')
    result: list[dict[str, str]] = []
    for block in re.split(r'(?m)^(?=name:)', run(*args)):
        fields: dict[str, str] = {}
        key = ''
        for line in block.splitlines():
            match = re.match(r'^([a-z-]+):\s*(.*)', line)
            if match:
                key = match[1]
                fields[key] = match[2]
            elif line.startswith(' ') and key:
                fields[key] += ' ' + line.strip()
        if fields:
            result.append(fields)
    return result


def ghc_notices(package: str, licenses: Path) -> str:
    """Copy this boot package's declared notices from the verified GHC source."""
    source = Path('/opt/ghc-source-notices')
    if package == 'rts':
        notices = [source / 'LICENSE', source / 'rts/linker/ELFRelocs/LICENSE-LLVM.TXT']
    elif package == 'libffi':
        notices = [source / 'libffi-tarballs/LICENSE']
    else:
        directory = source / 'libraries' / package
        candidates = [p for p in directory.rglob(package + '.cabal*')
                      if p.name in (package + '.cabal', package + '.cabal.in')]
        require(len(candidates) == 1, f'no unique GHC source package: {package}')
        cabal = candidates[0]
        field = re.search(r'(?im)^license-files?:[ \t]*(.*(?:\n[ \t]+[^\n]+)*)', cabal.read_text())
        require(field is not None, f'no declared GHC package notices: {package}')
        assert field is not None
        notices = [cabal.parent / name for name in shlex.split(field[1].replace(',', ' '))]
    require(bool(notices) and all(p.is_file() for p in notices), f'missing GHC notices: {package}')
    for notice in notices:
        target = licenses / 'ghc' / notice.relative_to(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(notice, target)
    return ','.join(str(p.relative_to(source)) for p in notices)


def main() -> None:
    """Copy the closure, collect notices, relocate ELF paths, and append the footer."""
    require(len(sys.argv) == 3, 'Usage: bundle-linux.py RAW_LIBRARY NEW_OUTPUT_DIRECTORY')
    source, output = Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve()
    require(not output.exists(), f'output already exists: {output}')
    output.mkdir(parents=True)
    libraries = output / 'lib'
    licenses = output / 'licenses'
    libraries.mkdir()
    licenses.mkdir()
    cabal_paths: dict[str, str] = json.loads(run('cabal', 'path', '--output-format=json'))
    packages = package_fields(Path(cabal_paths['store-dir']))
    listing = run('ldd', str(source))
    require('not found' not in listing, 'unresolved native dependency:\n' + listing)
    dependencies = [Path(p).resolve() for p in re.findall(r'=> (/\S+)', listing)]
    dependencies += [Path(p).resolve() for p in re.findall(r'^\s*(/\S+) \(', listing, re.M)]
    require(bool(dependencies), 'no dynamic dependencies resolved')
    records: list[dict[str, object]] = []
    host: list[str] = []
    identities: dict[str, str] = {}
    copied_notices: set[str] = set()
    for path in [source] + sorted(set(dependencies)):
        owner = ''
        bindist_native = str(path).startswith('/opt/ghc/') and not path.name.startswith('libHS')
        if not path.name.startswith('libHS') and path != source and not bindist_native:
            candidates = [str(path), str(path).replace('/usr/lib/', '/lib/', 1)]
            for candidate in candidates:
                found = subprocess.run(['dpkg-query', '-S', candidate], text=True, capture_output=True)
                if found.returncode == 0:
                    owner = found.stdout.split(': ', 1)[0]
                    break
            require(bool(owner), f'no native package provenance: {path}')
            if owner.split(':')[0] == 'libc6':
                host.append(path.name)
                continue
        info = metadata(path)
        name = 'haskell_demo.so' if path == source else path.name
        # Preserve the loader name even when the resolved file has a version suffix.
        if path != source:
            sonames = info['soname']
            require(len(sonames) == 1, f'missing SONAME: {path}')
            name = str(sonames[0])
        require(name not in identities or identities[name] == digest(path), f'duplicate library name: {name}')
        identities[name] = digest(path)
        target = output / name if path == source else libraries / name
        shutil.copyfile(path, target)
        provenance: dict[str, object] = {}
        if owner:
            package = owner.split(':')[0]
            notice = Path('/usr/share/doc') / package / 'copyright'
            require(notice.is_file(), f'missing native copyright: {owner}')
            notice_name = package + '-copyright'
            shutil.copyfile(notice, licenses / notice_name)
            common = sorted(set(re.findall(r'/usr/share/common-licenses/([A-Za-z0-9_.+-]+)', notice.read_text())))
            for license_name in common:
                license_name = license_name.rstrip('.')
                common_source = Path('/usr/share/common-licenses') / license_name
                require(common_source.is_file(), f'missing common license: {common_source}')
                (licenses / 'common').mkdir(exist_ok=True)
                shutil.copyfile(common_source, licenses / 'common' / license_name)
            provenance = {'package': owner, 'version': run('dpkg-query', '-W', '-f=${Version}', owner), 'notice': notice_name, 'common_notices': common}
        elif bindist_native:
            require(path.name == 'libffi.so.8', f'unmapped bindist native library: {path}')
            provenance = {'package': 'GHC 9.14.1 bundled libffi', 'notice': ghc_notices('libffi', licenses), 'source_archive_sha256': '2a83779c9af86554a3289f2787a38d6aa83d00d136aa9f920361dd693c101e77'}
        elif path == source:
            shutil.copyfile('/work/source/LICENSE', licenses / 'demo-LICENSE')
            demo_source = Path('/work/local-sources/duckdb-haskell-extension-example-0.1.0.0.tar.gz')
            require(demo_source.is_file(), 'missing demo source archive')
            shutil.copyfile(demo_source, licenses / demo_source.name)
            provenance = {'package': 'duckdb-haskell-extension-example', 'license': 'MPL-2.0', 'notice': 'demo-LICENSE', 'source_archive': demo_source.name}
        else:
            matches = [p for p in packages if any(name.startswith('lib' + lib + suffix) for lib in p.get('hs-libraries', '').split() for suffix in ('-ghc', '_thr-ghc'))]
            require(len(matches) == 1, f'no unique Haskell package provenance: {name}')
            package = matches[0]
            unit = package['name'] + '-' + package['version']
            if str(path).startswith('/opt/ghc/'):
                notice_name = ghc_notices(package['name'], licenses)
            else:
                archives = list(Path('/work/local-sources').glob(unit + '.tar.gz'))
                archives += list(Path(cabal_paths['remote-repo-cache']).glob('*/' + package['name'] + '/' + package['version'] + '/' + unit + '.tar.gz'))
                require(len(archives) == 1, f'no unique source archive: {unit}')
                with tarfile.open(archives[0]) as archive:
                    require(any(Path(m.name).name.upper().startswith(('LICENSE', 'COPYING')) for m in archive.getmembers()), f'no license text in source archive: {unit}')
                notice_name = unit + '.tar.gz'
                if notice_name not in copied_notices:
                    shutil.copyfile(archives[0], licenses / notice_name)
                    copied_notices.add(notice_name)
            provenance = {'package': unit, 'unit_id': package['id'], 'license': package['license'], 'notice': notice_name}
            if str(path).startswith('/opt/ghc/'):
                provenance['source_archive_sha256'] = '2a83779c9af86554a3289f2787a38d6aa83d00d136aa9f920361dd693c101e77'
        run('patchelf', '--set-rpath', '$ORIGIN/lib' if path == source else '$ORIGIN', str(target))
        require(run('patchelf', '--print-rpath', str(target)).strip() == ('$ORIGIN/lib' if path == source else '$ORIGIN'), f'bad relocated path: {target}')
        records.append({'path': str(target.relative_to(output)), **info, 'provenance': provenance})
    root = Path(__file__).resolve().parents[2]
    cli = os.environ['DUCKDB_CLI']
    platform = run(cli, '-csv', '-noheader', '-c', 'PRAGMA platform;').strip()
    require(platform.startswith('linux_amd64'), f'unsupported DuckDB platform: {platform}')
    os.chdir(root)
    run('runghc', str(root / 'dev/extensions/Pack.hs'), str(output / 'haskell_demo.so'), '--output', str(output / 'haskell_demo.duckdb_extension'), '--platform', platform, '--extension-version', '0.1.0')
    (output / 'haskell_demo.so').unlink()
    records[0]['path'] = 'haskell_demo.duckdb_extension'
    for record in records:
        record['sha256'] = digest(output / str(record['path']))
    notice_hashes = {str(p.relative_to(output)): digest(p) for p in sorted(licenses.rglob('*')) if p.is_file()}
    manifest = {'format': 1, 'target': 'Ubuntu 24.04+ Linux x86_64; one Haskell extension in native DuckDB 2.0.0 CLI',
                'unsupported': ['Haskell host executables', 'multiple Haskell extensions', 'other DuckDB releases', 'macOS', 'Windows'],
                'build_image': os.environ['BUNDLE_BUILD_IMAGE'], 'ghc': run('ghc', '--numeric-version').strip(),
                'cabal': run('cabal', '--numeric-version').strip(), 'duckdb_platform': platform,
                'ghc_source_archive_sha256': '2a83779c9af86554a3289f2787a38d6aa83d00d136aa9f920361dd693c101e77',
                'ghc_archive_sha256': '530f0861d1d7b45476c158a9e68aa89987687aaf9939df7a15995c45726e2344',
                'cabal_archive_sha256': 'c6385c155ff61f792dfed0e8c101004db63f8d8b556dd6c9838f81a012bf28a6',
                'native_archive_pins': (root / 'duckdb-ffi/native.sha256').read_text(),
                'host_glibc': sorted(set(host)), 'objects': records, 'license_sha256': notice_hashes}
    (output / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')


if __name__ == '__main__':
    main()
