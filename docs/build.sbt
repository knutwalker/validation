import sbt._
import Keys._
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import sbtunidoc.Plugin.UnidocKeys._

site.settings
ghpages.settings
unidocSettings

git.remoteRepo := "git@github.com:knutwalker/validation.git"
ghpagesNoJekyll := false
includeFilter in makeSite ~= (_ || "*.yml" || "*.md")

site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api")
siteMappings <++= baseDirectory map { dir =>
  val specs = dir / ".." / "target" / "specs2-reports"
  Path.allSubpaths(specs).map { case (f, p) => (f, s"guide/$p") }.toSeq
}
