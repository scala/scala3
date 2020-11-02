package dotty.dokka

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._
import org.jetbrains.dokka.transformers.documentation.PreMergeDocumentableTransformer
import org.jetbrains.dokka.transformers.pages.PageTransformer

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.{ DokkaConfiguration$DokkaSourceSet => DokkaSourceSet }
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import com.virtuslab.dokka.site.SourceSetWrapper
import com.virtuslab.dokka.site.JavaSourceToDocumentableTranslator
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer
import dotty.dokka.tasty.{DokkaTastyInspector, SbtDokkaTastyInspector}
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.pages._
import dotty.dokka.model.api._
import org.jetbrains.dokka.CoreExtensions
import com.virtuslab.dokka.site.StaticSitePlugin
import org.jetbrains.dokka.base.DokkaBase
import com.virtuslab.dokka.site.ExtensionBuilderEx
import java.util.{List => JList}

/** Main Dokka plugin for the doctool.
  *
  * Wires together classes responsible for consuming Tasty and generating
  * documentation.
  *
  * Most of the work of parsing Tasty is done by [[DokkaTastyInspector]].
  */
class DottyDokkaPlugin extends DokkaJavaPlugin:

  lazy val dokkaBase = plugin(classOf[DokkaBase])
  lazy val dokkaSitePlugin = plugin(classOf[StaticSitePlugin])

  val provideMembers = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getSourceToDocumentableTranslator)
    .fromInstance(EmptyModuleProvider)
    .overrideExtension(dokkaBase.getPsiToDocumentableTranslator)
  )

  // Just turn off another translator since multiple overrides does not work
  val disableDescriptorTranslator = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getSourceToDocumentableTranslator)
    .fromInstance(ScalaModuleProvider)
    .overrideExtension(dokkaBase.getDescriptorToDocumentableTranslator)
    .name("disableDescriptorTranslator")
  )

  // Clean up empty module provided in disableDescriptorTranslator
  val cleanUpEmptyModules = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getPreMergeDocumentableTransformer)
      .fromInstance(_.asScala.filterNot(_.getName.isEmpty).asJava)
  )

  val ourSignatureProvider = extend(
    _.extensionPoint(dokkaBase.getSignatureProvider)
      .fromRecipe(ctx =>
        new ScalaSignatureProvider(ctx.single(dokkaBase.getCommentsToContentConverter), ctx.getLogger)
      ).overrideExtension(dokkaBase.getKotlinSignatureProvider)
  )

  val scalaResourceInstaller = extend(
    _.extensionPoint(dokkaBase.getHtmlPreprocessors)
      .fromInstance(new ScalaResourceInstaller())
      .after(dokkaBase.getCustomResourceInstaller)
  )

  val scalaEmbeddedResourceAppender =  extend(
    _.extensionPoint(dokkaBase.getHtmlPreprocessors)
      .fromInstance(new ScalaEmbeddedResourceAppender())
      .after(dokkaBase.getCustomResourceInstaller)
      .name("scalaEmbeddedResourceAppender")
  )

  val scalaDocumentableToPageTranslator = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getDocumentableToPageTranslator)
      .fromRecipe(ctx => ScalaDocumentableToPageTranslator(
            ctx.single(dokkaBase.getCommentsToContentConverter),
            ctx.single(dokkaBase.getSignatureProvider),
            ctx.getLogger
      ))
      .overrideExtension(dokkaBase.getDocumentableToPageTranslator)
  )

  val packageHierarchyTransformer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getPageTransformer)
      .fromRecipe(PackageHierarchyTransformer(_))
      .before(dokkaBase.getRootCreator)
  )

  val inheritanceTransformer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getDocumentableTransformer)
      .fromRecipe(InheritanceInformationTransformer(_))
      .name("inheritanceTransformer")
  )

  val ourSourceLinksTransformer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getDocumentableTransformer)
      .fromRecipe(ctx => ScalaSourceLinksTransformer(
            ctx,
            ctx.single(dokkaBase.getCommentsToContentConverter),
            ctx.single(dokkaBase.getSignatureProvider),
            ctx.getLogger
        )
      )
  )

  val ourRenderer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getRenderer)
      .fromRecipe(ScalaHtmlRenderer(_))
      .overrideExtension(dokkaSitePlugin.getCustomRenderer)
  )

  val commentsToContentConverter = extend(
    _.extensionPoint(dokkaBase.getCommentsToContentConverter)
    .fromInstance(ScalaCommentToContentConverter)
    .overrideExtension(dokkaBase.getDocTagToContentConverter)
  )

  val implicitMembersExtensionTransformer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getDocumentableTransformer )
      .fromRecipe(ImplicitMembersExtensionTransformer(_))
      .name("implicitMembersExtensionTransformer")
  )

  val muteDefaultSourceLinksTransformer = extend(
    _.extensionPoint(CoreExtensions.INSTANCE.getPageTransformer)
    .fromInstance(new PageTransformer {
      override def invoke(root: RootPageNode) = root
    })
    .overrideExtension(dokkaBase.getSourceLinksTransformer)
    .name("muteDefaultSourceLinksTransformer")
  )

// TODO remove once problem is fixed in Dokka
extension [T]  (builder: ExtensionBuilder[T]):
  def before(exts: Extension[_, _, _]*):  ExtensionBuilder[T] =
    (new ExtensionBuilderEx).newOrdering(builder, exts.toArray, Array.empty)

  def after(exts: Extension[_, _, _]*):  ExtensionBuilder[T] =
    (new ExtensionBuilderEx).newOrdering(builder, Array.empty, exts.toArray)
