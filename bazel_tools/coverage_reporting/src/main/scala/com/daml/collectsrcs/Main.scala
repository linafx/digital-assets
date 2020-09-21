// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.collectsrcs

import java.io.File

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Insufficient parameters. Format: bazel run //bazel_tools/collectsrcs:run -- root_dir target_dir")
      println("Important: full absolute path needed! Suggestion: $(pwd)/something/something")
      System.exit(1)
    }
    val rootDir = args(0)
    val targetDir = if (args(1).endsWith("/")) args(1).dropRight(1) else args(1)

    println("Processing files")
    getRelevantRootDirs(new File(rootDir))
      .flatMap(getAllFilesInDir)
      .foreach {
        file =>
          val packagePath = getJavaOrScalaPathFromPackageDef(file)
          if (packagePath.startsWith("com/daml")) {
            val path = s"$targetDir/$packagePath"
            makePathIfNeeded(path)
            copy(file.getPath, s"$path/${file.getName}")
            print(".")
          } else {
            //println("Could not deduce package: ", file.getPath)
            print("X")
          }
      }
    println()
    println("Done")
  }

  val excludedFromRoot = Set(
    "bazel-bin",
    "bazel-out",
    "bazel-daml",
    "bazel-testlogs",
    "report",
    "dev-env",
    "docs"
  )

  val srcExtensions = Set(
    ".scala",
    ".java"
  )

  def getAll(dir: File): Iterator[File] =
    if (dir.exists && dir.isDirectory) dir.listFiles.iterator else Iterator.empty

  def getRelevantRootDirs(root: File): Iterator[File] =
    getAll(root)
      .filter(_.isDirectory)
      .filterNot(file => excludedFromRoot.exists(file.getPath.endsWith))

  def getAllFilesInDir(dir: File): Iterator[File] =
    getAll(dir).flatMap {
      case dir if dir.isDirectory => getAllFilesInDir(dir)
      case file if srcExtensions.exists(file.getPath.endsWith) => Iterator(file)
      case _ => Iterator.empty
    }

  def makePathIfNeeded(path: String): Unit = {
    new File(path).mkdirs()
    ()
  }

  def copy(from: String, to: String): Unit = {
    import java.io.{FileInputStream, FileOutputStream}
    val src = new FileInputStream(from)
    val dest = new FileOutputStream(to)
    dest.getChannel.transferFrom(
      src.getChannel, 0, Long.MaxValue)
    src.close()
    dest.close()
    ()
  }

  def getJavaOrScalaPathFromPackageDef(f: File): String = {
    val s = Source.fromFile(f)
    val result = s.getLines()
      .filter(_.startsWith("package "))
      .take(40)
      .map(_.drop("package ".length))
      .flatMap(_.split('.'))
      .map(_.trim)
      .map(_.filter(_.isLetterOrDigit))
      .filter(_.nonEmpty)
      .mkString("/")
    s.close()
    result
  }
}
