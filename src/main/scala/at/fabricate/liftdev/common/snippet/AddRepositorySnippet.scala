package at.fabricate.liftdev.common
package snippet

import model.AddRepository
import model.AddRepositoryMeta
import net.liftweb.util.CssSel
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.jquery.JqJsCmds.AppendHtml
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scala.xml.Text
import model.BaseEntity
import model.BaseMetaEntityWithTitleAndDescription
import java.io.File
import org.eclipse.jgit.revwalk.RevCommit
import lib.GitWrapper
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.jquery.JqJsCmds.DisplayMessage
import net.liftweb.http.js.JsCmds.Function
import net.liftweb.http.js.JE.JsVar
import model.BaseEntityWithTitleAndDescription
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import net.liftweb.http.RewriteResponse
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.*
import net.liftweb.sitemap.Loc.Hidden
import scala.collection.JavaConversions._
import org.eclipse.jgit.diff.DiffEntry


trait AddRepositorySnippet[T <: BaseEntityWithTitleAndDescription[T] with AddRepository[T]] extends BaseEntityWithTitleAndDescriptionSnippet[T] {
  
  var filesTemplate : NodeSeq = NodeSeq.Empty 
  var commitTemplate : NodeSeq = NodeSeq.Empty 

  def displayMessageAndHideLocal(message : String ) : JsCmd = displayMessageAndHide("repositoryMessages",message)
  
  
  abstract override def view(xhtml: NodeSeq) :  NodeSeq  =  {
    // get just the comment section
    filesTemplate = ("#listfiles ^^" #> "str").apply(xhtml)    
    commitTemplate = ("#listcommits ^^" #> "str").apply(xhtml)

    super.view(xhtml)
  }
  
  abstract override def asHtml(item : ItemType) : CssSel = {

      var commitLabel = ""
        
      			      var status = item.repository.getStatus()
			          var update = List(
			              "added: "->status.getAdded(),
			              "removed: "->status.getRemoved(),
			              "missing: "->status.getMissing(),
			              "changed: "->status.getChanged(),
			              "modified: "->status.getModified(),
			              "conflicting: "->status.getConflicting()
			              )
	    	  	def commitRepository(localItem : ItemType)() : JsCmd = {
		    		println("commiting the repository with label: "+commitLabel )
		        	localItem.repository.commit(commitLabel)
	    	  	    SetHtml("commits", getNewCommitList(localItem) ) &
	    	  	    displayMessageAndHideLocal("Commiting %s was successful!".format(commitLabel)) &
	    	  	     // clear the form
					JsCmds.SetValById("commitlabel", "")
	    	  	    
	    	  	}
		     
	    	  	 // update methods for the ajax stuff
	    	  	 def updateFileList(localItem : ItemType) : JsCmd = SetHtml("files", getNewFileList(localItem) ) 
	    	  	 
	    	  	 def getNewFileList(localItem : ItemType) : NodeSeq = ("#listfiles" #>  listAllFiles(localItem)).apply(filesTemplate) 
	    	  	 
	    	  	 def getNewCommitList(localItem : ItemType) : NodeSeq = ("#listcommits" #>  listAllCommits(localItem)).apply(commitTemplate) 

	    	  	 
	    	  	 def listAllFiles(localItem : ItemType) : List[CssSel] = 
	    	  	   localItem.repository.getAllFilesInRepository.map(
	    	  			   file => bindFileCSS(file,localItem) 
			          )
	    	  	 
	    	  	 def bindFileCSS(file : File, localItem : ItemType) : CssSel = 
	    	  	   "#filename" #> file.getName() & 
			          "#deletefile [onclick]" #> SHtml.ajaxInvoke(() => {
			            localItem.repository.deleteFileFromRepository(file)
			            updateFileList(localItem) &
			            displayMessageAndHideLocal("Deleted file "+file.getName())
			          } ) 
	    	  	   
	    	  	 def listAllCommits(localItem : ItemType) : List[CssSel] = 
	    	  	   localItem.repository.getAllCommits.map(
	    	  			   commit => bindCommitCss(commit, localItem) 
			          )
			          
			     def bindCommitCss(commit : RevCommit, localItem : ItemType) : CssSel ={
			          var diffs = if (commit.getParents().length > 0) {
			        	  			println(" commit has parents ")
			            localItem.repository.differenceBetweenCommits(commit, commit.getParents().head)
			          }
			        	  		else
			        	  		  List()
			          def prittyPrintDifference(difference : DiffEntry) : String = {
			        	  		  difference.getChangeType match {
			        	  		    case DiffEntry.ChangeType.ADD => "Added file %s to the repository".format(difference.getNewPath())
			        	  		    case DiffEntry.ChangeType.DELETE => "Deleted file %s from the repository".format(difference.getOldPath())
			        	  		    case DiffEntry.ChangeType.MODIFY => "Modified file %s".format(difference.getNewPath())
			        	  		    case DiffEntry.ChangeType.COPY => "Copied file %s to %s".format(difference.getOldPath(),difference.getNewPath())
			        	  		    case DiffEntry.ChangeType.RENAME => "Renamed file %s to %s".format(difference.getOldPath(),difference.getNewPath())  	  		    
			        	  		    case _ => difference.toString()
			        	  		  }
			        	  		}

//			          status.
			          "#committext" #> commit.getFullMessage() & 
			          "#commitcommitter" #> commit.getCommitterIdent().getName() & 
			          "#commitdifferencecount" #> diffs.length & 
			          "#commitdifferencedetails" #> <ul>{diffs.map(difference => <li>{prittyPrintDifference(difference)}</li>)}</ul> & 
//			          "#commitfooter" #> <ul>{commit.getFooterLines().toList.map(line => <li>{line.toString()}</li>)}</ul> & 
			          "#downloadcommit [href]" #> "/%s/%s/%s/%s/%s.zip".format(
			              localItem.basePathToRepository, 
			              localItem.repositoryID,
			              localItem.endPathToData,
			              commit.getName(),
			              localItem.repositoryID) & 
			          "#revertcommit [onclick]" #> SHtml.ajaxInvoke(() => {
			            localItem.repository.revertChangesOfCommit(commit)
			            updateFileList(localItem) &
			            displayMessageAndHideLocal("Undo changes of commit "+commit.getFullMessage())
			          } )& 
			          "#resetcommit [onclick]" #> SHtml.ajaxInvoke(() => {
			            localItem.repository.resetToCommit(commit)
			            updateFileList(localItem) &
			            displayMessageAndHideLocal("Rolled back to commit "+commit.getFullMessage())
			          } )
			          
			       }
			          
				(

		      "#commitlabel" #> SHtml.ajaxText("", value => {commitLabel = value}, "placeholder"->"Describe the key features of this project revision") &
			  "#listfiles" #>  listAllFiles(item ) &
			  "#listcommits" #> listAllCommits(item ) &	
			  "#repositorystatus" #> <ul>{update.map({case (typeOfUpdate, listOfUpdate) => <li>{typeOfUpdate+listOfUpdate.mkString("",", ","") }</li>})}</ul> & 

		      "#commitrepo [onclick]" #> SHtml.ajaxInvoke(commitRepository(item) ) &
		      "#fileupload [data-url]" #> "/%s/%s/%s/%s".format(
		          item.apiPath, 
		          item.uploadPath,
		          item.repositoryPath,
		          item.primaryKeyField) &
		      "#uploadconfig *+" #> Function(
			        "UpdateFilelist", List("newfile"),SHtml.ajaxCall(JsVar("newfile"), 
				            (newfile: String) => {
				              println("received %s and called snipped ".format(newfile)); 
				              displayMessageAndHideLocal("Uploading %s was successful!".format(newfile)) &
				              updateFileList(item ) 
				              }
			            )._2.cmd
		    		  )
				) &
     // chain the css selectors 
     (super.asHtml(item))
  }
  
}
