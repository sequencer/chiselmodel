// import Mill dependency
import mill._
import mill.modules.Util
import scalalib._
// support BSP
import mill.bsp._
// support Scalafmt
import mill.scalalib.scalafmt._
// input build.sc from each repositories.
import $file.dependencies.chisel3.build
import $file.dependencies.firrtl.build
import $file.dependencies.treadle.build

// Global Scala Version
val sv = "2.12.12"

// Init local repositories from repositories if exist a build.sc
object myfirrtl extends dependencies.firrtl.build.firrtlCrossModule(sv) {
  override def millSourcePath = os.pwd / "dependencies" / "firrtl"
}

object mychisel3 extends dependencies.chisel3.build.chisel3CrossModule(sv) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)

  def treadleModule: Option[PublishModule] = Some(mytreadle)
}

object mytreadle extends dependencies.treadle.build.treadleCrossModule(sv) {
  override def millSourcePath = os.pwd /  "dependencies" / "treadle"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}

object chiselmodel extends ScalaModule with ScalafmtModule { m =>
  override def scalaVersion = sv

  override def moduleDeps = super.moduleDeps ++ Seq(mychisel3, macros)

  override def scalacPluginClasspath = super.scalacPluginClasspath() ++ Agg(
    mychisel3.plugin.jar()
  )

  // add some scala ivy module you like here.
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle:latest.integration",
    ivy"com.lihaoyi::os-lib:latest.integration",
    ivy"com.lihaoyi::pprint:latest.integration",
    ivy"org.scala-lang.modules::scala-xml:latest.integration"
  )

  object macros extends ScalaModule with ScalafmtModule {
    override def scalaVersion = m.scalaVersion
    override def ivyDeps = m.ivyDeps() ++ Seq(
      ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}"
    )
  }

  // use scalatest as your test framework
  object tests extends Tests with ScalafmtModule {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:latest.integration")

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
