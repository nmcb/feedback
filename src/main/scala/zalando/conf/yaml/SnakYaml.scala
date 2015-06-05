package zalando.conf.yaml

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.introspector.BeanAccess
import scala.beans.BeanProperty

object SnakeYaml {

  val text =
    """
      | password: PASSWORD
      | mailbox: INBOX
      | accountName: Ymail Account
      | username: USERNAME
      | imapServerUrl: imap.mail.yahoo.com
      | protocol: imaps
      | minutesBetweenChecks: 1
      | usersOfInterest: [barney, betty, wilma]
    """.stripMargin

  def testBean() = {
    val yaml = new Yaml(new Constructor(classOf[EmailAccountBean]))
    println(yaml.load(text).asInstanceOf[EmailAccountBean])
  }

  def testCaseClass() = {
    val yaml = new Yaml(new Constructor(classOf[EmailAccountCase]))
    yaml.setBeanAccess(BeanAccess.FIELD)
    println(yaml.load(text).asInstanceOf[EmailAccountCase]) // BOOM! no no-args constructor
  }
}

/**
 * The SnakeYAML Constructor approach requires a no-args constructor.
 */
class EmailAccountBean {
  @BeanProperty var accountName         : String = null
  @BeanProperty var username            : String = null
  @BeanProperty var password            : String = null
  @BeanProperty var mailbox             : String = null
  @BeanProperty var imapServerUrl       : String = null
  @BeanProperty var minutesBetweenChecks: Int    = 0
  @BeanProperty var protocol            : String = null
  @BeanProperty var usersOfInterest              = new java.util.ArrayList[String]()

  override def toString: String = {
    s"acct $accountName, user $username, url $imapServerUrl"
  }
}

case class EmailAccountCase(
  accountName: String,
  username: String,
  password: String,
  mailbox: String,
  imapServerUrl: String,
  minutesBetweenChecks: Int,
  protocol: String, usersOfInterest: List[String]
)
