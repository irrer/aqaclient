/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqaclient

import java.io.File

/**
  * Data for upload to the server
  *
  * @param procedure The procedure to execute on the server.
  * @param description This is used for logging only.
  * @param zipFile File containing zipped data to upload.
  */
class UploadSet(procedure: Procedure, description: String, zipFile: File) {

  def this(procedure: Procedure, description: String, fileList: Seq[File]) {
    this(procedure = procedure, description = description, zipFile = ClientUtil.makeZipFile(fileList))
  }

  def this(procedure: Procedure, seriesList: Seq[org.aqaclient.Series], description: String) {
    this(procedure, description, ClientUtil.makeZipFile(seriesList.flatMap(s => ClientUtil.listFiles(s.dir))))
  }

  override def toString: String = {
    procedure.toString + " :: " + description + " zip file: " + zipFile.getAbsolutePath + "   zip file size: " + zipFile.length()
  }

  def j0(a: Seq[Int]) = "hey"
  def j0(a: Seq[String]) = "ho"

  /**
    * Execute this function after the upload has completed.
    *
    * @param pass True if upload was successful.  This is independent o the execution of the procedure, which
    *             may subsequently pass, fail, crash, timeout, or whatever.
    */
  def postProcess(pass: Boolean): Unit = {}
}
