"""Regressions for instance permalink checks."""

import io
import tarfile
import tempfile
import unittest
from pathlib import Path

from check_links import Page, check_archive


class InstanceLinks(unittest.TestCase):
    """Check links to instance details rather than class definitions."""

    def page(self, href: str, details: bool = True) -> Page:
        """Make the HTML structure that Haddock uses for an instance."""
        return Page(
            '<a id="t:C"></a><table><tr><td class="src clearfix">'
            '<span class="inst-left"><span class="instance details-toggle" '
            'data-details-id="i:id:T:C:1"></span><a href="Class.html#t:C">C</a> T</span>'
            f'<a class="selflink" href="{href}">#</a></td></tr>'
            + ('<tr><td><details id="i:id:T:C:1"></details></td></tr>' if details else "")
            + "</table>"
        )

    def test_instance_target(self) -> None:
        self.assertEqual(self.page("#i:id:T:C:1").broken(), [])

    def test_class_target_is_not_instance_permalink(self) -> None:
        self.assertEqual(self.page("#t:C").broken(), [("i:id:T:C:1", "#t:C")])

    def test_missing_details(self) -> None:
        self.assertEqual(self.page("#i:id:T:C:1", details=False).broken(), [("i:id:T:C:1", "#i:id:T:C:1")])

    def test_operator_method_permalink(self) -> None:
        page = Page(
            '<details id="i:id:T:C:1"><div class="subs methods">'
            '<p class="src" id="i:id:T:C:1:v:-37--37-">'
            '<a href="Class.html#v:-37--37-">(%%)</a>'
            '<a class="selflink" href="#i:id:T:C:1:v:-37--37-">#</a></p></div></details>'
        )
        self.assertEqual(page.broken(), [])
        self.assertEqual(page.method_headers, 1)

    def test_duplicate_method_ids(self) -> None:
        page = Page('<p id="i:id:T:C:1:v:method"></p><p id="i:id:T:C:1:v:method"></p>')
        self.assertEqual(page.duplicates, {"i:id:T:C:1:v:method"})

    def test_missing_permalink(self) -> None:
        page = Page(
            '<td><span class="instance" data-details-id="i:id:T:C:1"></span></td>'
            '<details id="i:id:T:C:1"></details>'
        )
        self.assertEqual(page.broken(), [("i:id:T:C:1", "<0 permalinks>")])

    def test_missing_method_permalink(self) -> None:
        page = Page(
            '<details id="i:id:T:C:1"><div class="subs methods">'
            '<p class="src" id="i:id:T:C:1:v:method">method</p></div></details>'
        )
        self.assertEqual(page.broken(), [("i:id:T:C:1:v:method", "<0 permalinks>")])

    def test_missing_local_page(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "docs.tar.gz"
            content = b'<a href="Missing.html#t:C">C</a>'
            with tarfile.open(path, "w:gz") as archive:
                member = tarfile.TarInfo("docs/Class.html")
                member.size = len(content)
                archive.addfile(member, io.BytesIO(content))
            report, failed = check_archive(path)
        self.assertTrue(failed)
        self.assertEqual(
            report["broken"],
            [{"page": "docs/Class.html", "target": "docs/Missing.html", "error": "missing local page"}],
        )


if __name__ == "__main__":
    unittest.main()
