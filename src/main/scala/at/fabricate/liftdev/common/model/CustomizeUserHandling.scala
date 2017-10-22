package at.fabricate.liftdev.common
package model

import net.liftweb.proto.{ProtoUser => GenProtoUser}
import scala.xml.NodeSeq
import net.liftweb.mapper.MetaMegaProtoUser
import net.liftweb.mapper.MegaProtoUser
import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer.From
import net.liftweb.util.Mailer.Subject
import net.liftweb.util.Mailer.To
import net.liftweb.util.Mailer.BCC
import net.liftweb.common.Empty
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer.MailBodyType
import scala.xml.Elem

trait CustomizeUserHandling[T <: MegaProtoUser[T] with BaseEntityWithTitleAndDescription[T]] extends MetaMegaProtoUser[T]  {
  self: T =>
    
    
  def customValidateUser(id: String): NodeSeq = findUserByUniqueId(id) match {
    case Full(user) if !user.validated_? =>
      user.setValidated(true).resetUniqueId().save
      logUserIn(user, () => {
        S.notice(S.?("account.validated"))
        S.redirectTo(homePage)
      })

    case _ => S.error(S.?("invalid.validation.link")); S.redirectTo(homePage)
  }
  
    /**
   * Send validation email to the user.  The XHTML version of the mail
   * body is generated by calling signupMailBody.  You can customize the
   * mail sent to users by override generateValidationEmailBodies to
   * send non-HTML mail or alternative mail bodies.
   */
  def customSendValidationEmail(user: TheUserType, customValidationPath : List[String]) {
    val resetLink = S.hostAndPath+"/"+customValidationPath.mkString("/")+
    "/"+urlEncode(user.getUniqueId())

    val email: String = user.getEmail

    val msgXml = signupMailBody(user, resetLink)

    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
                    (To(user.getEmail) :: 
                     generateValidationEmailBodies(user, resetLink) :::
                     (bccEmail.toList.map(BCC(_)))) :_* )
  }
  
  override def signupMailBody(user: TheUserType, validationLink: String): Elem = {
    (<html>
        <head>
          <title>{S.?("sign.up.confirmation")}</title>
        </head>
        <body>
          <p>{S.?("dear")} {user.defaultTranslation.obj.map { userTranslation => userTranslation.title.get }.openOr("...")},
            <br/>
            <br/>
            {S.?("sign.up.validation.link")} 
            <br/>
						<a href={validationLink}>{validationLink}</a>
            <br/>
            <br/>
            {S.?("thank.you")}
            <br/>
            {S.?("signature")}
            <br/>
            <br/>
            {S.?("mail.ps")}
          </p>
        </body>
     </html>)
  }
  
   override def passwordResetMailBody(user: TheUserType, resetLink: String): Elem = {
    (<html>
        <head>
          <title>{S.?("reset.password.confirmation")}</title>
        </head>
        <body>
          <p>{S.?("dear")} {user.defaultTranslation.obj.map { userTranslation => userTranslation.title.get }.openOr("...")},
            <br/>
            <br/>
            {S.?("click.reset.link")}
            <br/><a href={resetLink}>{resetLink}</a>
            <br/>
            <br/>
            {S.?("thank.you")}
            <br/>
            {S.?("signature")}
            <br/>
            <br/>
            {S.?("mail.ps")}
          </p>
        </body>
     </html>)
  }

    
  def customLostPassword(selector : ( (String, List[String],List[String]) => Unit ) => NodeSeq, defaultRedirectLocation : String = homePage) = {

    selector(customSendPasswordReset(_,_,_,defaultRedirectLocation))
  }
  
  def customSendPasswordReset(email: String, customPasswordResetPath: List[String], customValidationPath : List[String], defaultRedirectLocation : String = homePage) {
    findUserByUserName(email) match {
      case Full(user) if user.validated_? =>
        user.resetUniqueId().save
        val resetLink = S.hostAndPath+
        customPasswordResetPath.mkString("/", "/", "/")+urlEncode(user.getUniqueId())
        val email: String = user.getEmail

        Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
                        (To(user.getEmail) ::
                         generateResetEmailBodies(user, resetLink) :::
                         (bccEmail.toList.map(BCC(_)))) :_*)

        S.notice(S.?("password.reset.email.sent"))
        S.redirectTo(defaultRedirectLocation)

      case Full(user) =>
        customSendValidationEmail(user,customValidationPath)
        S.notice(S.?("account.validation.resent"))
        S.redirectTo(defaultRedirectLocation)

      case _ => S.error(userNameNotFoundString)
    }
  } 
  
  def customPasswordReset(selector : (T, ()=>Unit ) => NodeSeq, userid : String, defaultRedirectLocation : String = homePage) =
	  findUserByUniqueId(userid) match {
	    case Full(user) =>
	      def finishSet() {
	        user.validate match {
	          case Nil => S.notice(S.?("password.changed"))
	            user.resetUniqueId().save
	            logUserIn(user, () => S.redirectTo(defaultRedirectLocation))
	
	          case xs => S.error(xs)
	        }
	      }
	      
	      selector(user,finishSet)
	    case _ => S.error(S.?("password.link.invalid")); S.redirectTo(homePage)
	  }       
	  
   def customSignup(selector : (T, () => Unit ) => NodeSeq, customValidationPath : List[String], createUserInstance : () => TheUserType = createNewUserInstance,defaultRedirectLocation : String = homePage) = {
	  val theUser: TheUserType = mutateUserOnSignup(createUserInstance())
	  val theName = signUpPath.mkString("")
	  def testSignup() {
	  	validateSignup(theUser) match {
	  		case Nil =>
	  			customActionsAfterSignup(theUser, customValidationPath,() => S.redirectTo(defaultRedirectLocation))
	  		case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
	  	}
  	 }
  	 def innerSignup = {
  			 selector(theUser, testSignup)
  	 }
  	 innerSignup
  }
   
     /**
   * Override this method to do something else after the user signs up
   */
  def customActionsAfterSignup(theUser: TheUserType, customValidationPath : List[String], func: () => Nothing): Nothing = {
    theUser.setValidated(skipEmailValidation).resetUniqueId()
    theUser.save
    if (!skipEmailValidation) {
      customSendValidationEmail(theUser, customValidationPath)
      S.notice(S.?("sign.up.message"))
      func()
    } else {
      logUserIn(theUser, () => {      
        S.notice(S.?("welcome"))
        func()
      })
    }
  }
  
  
	def customChangePassword(selector : (T, (String, List[String]) => Unit  ) => NodeSeq, defaultRedirectLocation : String = homePage) = {
		val user = currentUser.openOrThrowException("we can do this because the logged in test has happened")

		def testAndSet(oldPassword : String, newPassword : List[String])() = {
			if (!user.testPassword(Full(oldPassword))) S.error(S.?("wrong.old.password"))
			else {
				user.setPasswordFromListString(newPassword)
				user.validate match {
					case Nil => user.save; S.notice(S.?("password.changed")); S.redirectTo(defaultRedirectLocation)
					case xs => S.error(xs)
				}
			}
		}

		selector(user, testAndSet)
	}
	  
  def customLogin(selector : NodeSeq, defaultRedirectLocation : String = homePage) = {
      if (S.post_?) {
    	S.param("username").
    		flatMap(username => findUserByUserName(username)) match {
    			case Full(user) if user.validated_? &&
    						user.testPassword(S.param("password")) => {
    								val preLoginState = capturePreLoginState()
    								val redir = loginRedirect.get match {
    									case Full(url) =>
    										loginRedirect(Empty)
    										url
    									case _ =>
    										defaultRedirectLocation
    								}
    								logUserIn(user, () => {
    									S.notice(S.?("logged.in"))
    									preLoginState()
    									S.redirectTo(redir)
    								})
    						}
    			case Full(user) if !user.validated_? =>
    				S.error(S.?("account.validation.error"))
    			case _ => S.error(S.?("invalid.credentials"))
    	}
      }

      selector
 }
  
  def customLogout(redirectLocation : String = homePage) = {
    logoutCurrentUser
    S.redirectTo(redirectLocation)
  }
    									
}
