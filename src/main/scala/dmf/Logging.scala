package dmf

// we need the typesafe Logger; our direct logging interface in Scala
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import ch.qos.logback.core.util.StatusPrinter
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.spi.PropertyDefiner
import ch.qos.logback.core.spi.ContextAwareBase
import scalax.file.ImplicitConversions._

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.sift.Discriminator


class LogDirPropertyDefiner extends ContextAwareBase with PropertyDefiner {

  def getPropertyValue() = ("logs").path

  /**
   * A property definer can also receive data _from_ the XML:
   * For instance if the <define> element has another element <MyKey>
   * then logback tries to find a function called "setMyBack: String => Unit"
   * which is called before getPropertyValue.
   * Name is case IN-sensitive
   * 
   * Example:
  def setMyKey(k: String) {
    println("key: " + k)   
    k
  }
   */
}

