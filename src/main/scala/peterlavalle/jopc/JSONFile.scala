package peterlavalle.jopc

import java.io.{File, FileInputStream, FileWriter}

import org.json.{JSONObject, JSONTokener}

trait JSONFile {
	def apply[O](act: JSONObject => O): O

	/**
	 * build a sub-object ... thing ...
	 */
	def /(path: String): JSONFile = {
		val real: JSONFile = this
		new JSONFile {
			override def apply[O](act: JSONObject => O): O =
				real {
					real: JSONObject =>
						act(real / path)
				}
		}
	}
}

object JSONFile {

	def apply(file: File): JSONFile =
		if (!file.isAbsolute)
			apply(file.getAbsoluteFile)
		else
			new JSONFile {

				val data: JSONObject = {
					if (!file.exists())
						new JSONObject()
					else
						new JSONObject(new JSONTokener(new FileInputStream(file)))
				}

				override def apply[O](act: JSONObject => O): O =
					data.synchronized {

						val out: O = act(data)

						val parent: File = file.getParentFile.getAbsoluteFile
						require(parent.isDirectory || parent.mkdirs())

						new FileWriter(file)
							.append(data.toString(1))
							.close()
						out
					}
			}
}
