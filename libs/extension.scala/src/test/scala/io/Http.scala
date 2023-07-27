package io

import extension.io.HTTP

object Http {

  def main(args: Array[String]): Unit = {
    HTTP.request(uri = "abc")
  }

}
