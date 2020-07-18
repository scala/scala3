rootProject.name = "dokkaJavaApi"

pluginManagement {
    val kotlin_version: String by settings
    plugins {
        id("org.jetbrains.kotlin.jvm") version kotlin_version
        id("com.github.johnrengelman.shadow") version "5.2.0"
        id("com.jfrog.bintray") version "1.8.5"
        id("com.gradle.plugin-publish") version "0.12.0"
    }

    repositories {
        maven("https://dl.bintray.com/kotlin/kotlin-eap/")
        maven("https://dl.bintray.com/kotlin/kotlin-dev/")
        mavenCentral()
        jcenter()
        gradlePluginPortal()
    }
}