<TITLE>Collection of Emacs Development Environment Tools Homepage</TITLE>

<?php
  include ("rightcol.php")
?>

<h2>Collection of Emacs Development Environment Tools</h2>

<P>Welcome to the CEDET homepage.  CEDET is a collection of tools
   written with the end goal of creating an advanced development
   environment.  CEDET is hosted at
   <a href=http://www.sourceforge.net>Source Forge</a>.  You can view
   CEDET's CVS archive, and project summary
   <a href=http://www.sourceforge.net/projects/cedet>here.</a>
</p>

<P>Emacs already is a great environment for writing software, but
   there are additional areas that need improvement.  Many new ideas
   for integrated environments have been developed in newer products,
   such as Microsoft's Visual environment.  CEDET is a new project
   which brings together the various tools I've built to replicate
   features.
</p>

<p>One such area previously lacking was a single integrated file/tag
   browsing tool.  This can be done in different ways.  One technique
   popular in new environments is called an explorer.  <a
   href=speedbar.shtml>Speedbar</a> is my replacement which is already
   a popular addition to Emacs and XEmacs.
</p>

<P>A second useful tool is Makefile Generation and Project Management.
   <a href=ede.shtml>EDE</a> is my solution to this problem. EDE
   attempts to not only solve that single set of problems, but to
   create a framework within which experts in specific languages can
   plug in support for thier envornments.
</p>

<P>Building any tool with that much flexibility requires an API with
   lots of configurability.  A CLOS like tool was needed, and <a
   href=eieio.shtml>EIEIO</a> is my emulation.  EIEIO provides a
   classic Object Oriented system.  The base EDE system is a set of
   classes and methods.  New languages can inherit from the base EDE
   classes, and implement methods they need for their desired
   environment.
</p>

<P>A third useful tool has the odd name of Intellisense in other
   development tools.  This basically means that the environment can
   identify the current editing context, and can provide useful
   information.  This could be as simple as a list of completions, or
   as complex as recommendations on arguments to pass to a function.
   My solution to this is a tool called QuickPeek.  Quickpeek is not
   currently listed on this page.<br>
   <b>Note:</b>I am probably going to abandon QuickPeek in favor of
   emerging tools in Semantic's Senator tool, maintained by David
   Ponce.
</p>

<P>Context recommendations are not possible without first having
   complete knowledge of the current project source code.  <a
   href=ede.shtml>EDE</a> provides project information.  To get
   information related to the sourcecode, however, a parser is needed.
   Since the goal is to support many languages, a parser generator is
   needed to create parsers easily.  The <a href=semantic.shtml>
   Semantic Bovinator</a> is CEDET's source code tag parser.
</p>

<P>Last but not least are a new set of emerging CASE tools which link
   diagrams and source code.  Modify the code, and your class
   hierarchy updates.  Modify the diagram, and the code updates.  I'm
   going to start a new project 
   <a href=cogre.shtml>COGRE</a> (COnnected GRaph Editor) for
   working with UML diagrams that are directly linked to source code.
   This will depend on the Semantic parser for reverse engineering,
   EIEIO for graph management, Speedbar for graph navigation, and
   Emacs 21, or XEmacs 20 for the graphical power needed to represent
   these things.
</p>

<P>Please visit individual project pages for additionalinformation and
   downloads.
</p>

<H3>Articles</h3>

<P>An
   <a href=http://www-106.ibm.com/developerworks/library/j-emacs/?n-j-5241>
   article</a>
   about the
   <a href=http://jde.sunsite.dk/>JDE</a>
   includes some pointers to CEDET, and metions some of these tools.</p>

<H3>Base Tools are:</h3>

<P><A HREF="semantic.shtml">The <B>Semantic Bovinator</B></A> is a
   parser generator.  It creates parsers in Emacs Lisp.  Includes
   interfaces to imenu, which-func, and speedbar to improve tag
   browsing.
</P>

<P><A HREF="eieio.shtml"> <B>EIEIO: Enhanced Implementation of Emacs
   Interpreted Objects</B></A> is a package which implements a
   <B>CLOS</B> subset for Emacs.  It includes examples which can draw
   simple tree graphs, and bar charts.
</p>

<P><A HREF="ftp/working.el-1.3.gz"> <b>Working</b></A> is a
   busy-meter utlity.  It displays eye candy while waiting for emacs
   to do something.
</p>

<h3>User Interface Tools are:</h3>

<P><A HREF="speedbar.shtml"><B>Speedbar</B></A> allows you to create a
   special skinny frame with a specialized directory listing in it.
   Speedbar is a manual browser, class browser, file system browser,
   project browser, and a whatsis browser.
</P>

<P><A href="ede.shtml"> <B>EDE: Emacs Development Environment</b></a>
   implements projects under emacs making it easy to maintain programs
   without learning make.  This depends on EIEIO.</p>

<P><A href="cogre.shtml"> <b>COGRE: COnnected GRaph Editor</b></a>
   pronounced like <em>cougar</em>, is an interface to managing connected
   graphs, such as UML class diagrams.</p>
   

<h3>Tools hosted elsewhere:</h3>

<P><a href=http://home.swipnet.se/mayhem/ecb.html>
   <b>Emacs Class Browser (ECB)</b></a>
   lets you browse your files' contents.
   Uses the Semantic package.</p>

<h3>Other Miscelaneous Emacs Hacks at this web site:</h3>

<P><A HREF="checkdoc.shtml"><B>Checkdoc</B></A> is a program which
   checks the style of your documentation strings and comments.
   Useful if you want to keep other Emacs Lisp gurus from picking on
   you.
</P>

<P><A HREF="cparse.shtml"><B>C-Parse</B></A> is an <B>Emacs Lisp</B>
   program which can parse a c file, and allows searching for
   functions, variables, and types.  CParse is no longer supported.
   It's tools will be ported to Semantic.
</P>

<P><A HREF="download/X-0.3a.tar.gz"><b>X11 lib calls for Emacs, V
   0.3a</b></A> Imagine the binary network interface for X windows
   implemented in Emacs Lisp.  Is it useful?  Silly?  I dunno, but it
   was fun to play with.  No documentation.  Byte compile it, load
   "xhello.el" and run the function `XX' for the simple demo.<br>
</p>

<P><A HREF="download/hangman.el-0.1.gz"><b>Hangman</b></A> game for
   emacs.  About as simple as it gets.
</p>

<?php
  include ("footer.fsf.shtml")
?>
