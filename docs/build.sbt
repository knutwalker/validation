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
autoAPIMappings := true
scalacOptions in (ScalaUnidoc, unidoc) ++= List(
  "-doc-title", githubProject.value.repo,
  "-doc-version", version.value,
  "-diagrams",
  "-groups"
)

siteMappings <++= (mappings in (ScalaUnidoc, packageDoc), baseDirectory, version) map { (docs, dir, ver) =>
  val versions = Seq("latest", ver)
  val doc = for(v <- versions; (f, d) <- docs) yield (f, s"api/$v/" + d)
  val specs = dir / ".." / "target" / "specs2-reports"
  val guide = Path.allSubpaths(specs).map { case (f, p) => (f, s"guide/$p") }.toSeq
  doc ++ guide
}
