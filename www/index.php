<!-- -*- html -*- -->
<TITLE>Collection of Emacs Development Environment Tools Homepage</TITLE>

<?php
  include ("pagestart.php")
?>

<table width=100% class=BAR><tr><td>
<H3>What is <b>CEDET</b> ?</h3>
</td></tr></table>

<!-- The below paragraph gets sucked into aggregators.  Include some
     nice URLs in it to get folks back here. -->
<p><b>CEDET</b> is a <em><b>C</b>ollection
   of <b>E</b>macs <b>D</b>evelopment <b>E</b>nvironment <b>T</b>ools</em>
   written with the end goal of creating an advanced development
   environment in Emacs.  <b>CEDET</b> is hosted at
   <a href=http://www.sourceforge.net>Source Forge</a> and
   is <a href="http://www.gnu.org/philosophy/free-sw.html">Free
   Software</a>.  You can view <b>CEDET</b>'s CVS archive, project
   summary, and mailing lists at
   the <a href="http://www.sourceforge.net/projects/cedet"><b>CEDET</b>
   Project page.</a>
</p>

<h4>Why is <b>CEDET</b> needed?</h4>

<P>Emacs already is a great environment for writing software, but
   there are additional areas that need improvement.  Many new ideas
   for integrated environments have been developed in newer products,
   such as Microsoft's Visual environment, JBuilder, or Eclipse.
   CEDET is a project which implements several advanced features
   developers have come to expect from an Editor.
</p>


<table width=100% class=BAR><tr><td>
<H3>CEDET: A User's View</h3>
</td></tr></table>

<p>So what does an Emacs User get out of <b>CEDET</b>?  <b>CEDET</b>
   includes the following major user features:

</p>
<img src="img-gen/cedet-project-menu.png" align=right> 
<h4>Projects</h4>

<p>
   The CEDET <a href="cedet-project-menu.png">Project management system</a>
   provides a few simple keystrokes for organizing your files,
   building Makefiles or Automake files, and compiling your sources.

<p>If you don't want <b>CEDET</b> to manage your Makefiles, CEDET will
   still be able to identify some types of projects based on
   pre-existing build configurations, such as the Emacs sources, the
   Linux kernel, or any project built using Automake.

<p>The entire <b>CEDET</b> Makefile tree was built with <b>CEDET</b>'s
   project management system, so when you download and build the
   distribution package, you will be using a <b>CEDET</b> project.

<p>The image to the right shows a part of the Project management menu.
<br clear=all>

</p>
<img src="img-gen/semantic-ia-complete-menu.png" border=1 align=right>
<h4>Smart Completion</h4>

<p>Write code with <a href="intellisense.shtml">smart code completion</a>,
   sometimes known as "Intellisense".  The CEDET infrastructure for
   parsing and tagging files, and analyzing source code is one of the
   most accurate completion tools for C++ anywhere.  It will correctly
   handle inheritance scoping rules, templates, smart-pointers, and
   automatically filters based on what the value will be assigned into.

<p>Code completion is not restricted to just C or C++.  The completion
   engine is generic and works for any language that has a robust
   tagging parser written for CEDET, and a thin adaptation layer for
   the language.

<p>The image to the right shows smart completion configured to use
   tooltips in a CEDET unit test source file going through a templated
   smart pointer.
<br clear=all>

<h4>Symbol References</h4>

<p><img src="img-gen/symref.jpg" border=1 align=right>Analyze
   where <a href="symref.shtml">symbols are referenced</a> in your
   code base.  The Symbol References tool in <b>CEDET</b> can use
   external tools such as GNU Global, ID Utils, CScope, or even
   find/grep to locate usages of your symbols.  Every location is
   analyzed and displayed hierarchically showing you the file and
   function the reference occurs in, instead of a flat list of
   locations.

<p>The image to the right shows integration with GNU Global running in
   the GNU Global source code.

<br clear=all>

<h4>Code Generation</h4>

<p><img src="img-gen/srecode-fields.jpg" border=1 align=right>
   Generate code with a
   powerful <a href="codegen.shtml">language-independent template
   system</a>.  The template system in <b>CEDET</b> is a framework
   designed for code-generating application developers.  Even so, the
   base system and templates can also be used to insert small
   repetitive code blocks.  The template language is straight-forward,
   and flexible.  Because it uses the same code analytics as the Smart
   completion, it often can correctly guess a wide range of values to
   be used in your templates, saving you typing.

<p>Some pre-existing tools that use <b>CEDET</b>'s code generation
   system will write texinfo documentation for you, create doxygen
   style comments fully filled out from the local context, or insert
   get/set methods for variables in a class.

<p>The image to the right shows a simple class declaration inserted
   with the experimental "fields" feature enabled.  Typing in one
   field updates the others that match the same template dictionary
   macro.

<br clear=all>

<h4>UML Diagrams</h4>

<p><img src="img-gen/cogre-quick-class.jpg" border=1 align=right>
   Create UML diagrams either by hand, or automatically generate
   simple 3-tier class diagrams from your sources.  The diagrams are
   linked to your source-code, so you can browse quickly through you
   code from the convenience of UML.

<p>The image to the right shows <b>CEDET</b>'s UML tool's source code,
   called 'COGRE', diagramming and browsing itself.  Optional unicode
   character support is enabled for the special characters.
    
<br clear=all>

<h4>Install and Configure CEDET</h4>

<p><img src="img-gen/setup.jpg" border=1
   align=right><a href="setup.shtml">Setting up a tool as large and
   complex as <b>CEDET</b></a> can seem daunting.  Fortunately, a
   minimal <b>CEDET</b> initialization can take up only a couple lines
   of code in your <tt>.emacs</tt> file.  If you are brave and enjoy
   customizing your <b>CEDET</b> past this, then you are in luck as
   there are more options and small features to play with than anyone
   sane person might want.

<P>Alex Ott has written a great article
   called <a href="http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html">
   A Gentle Introduction to CEDET</a> that shows both the
   simple <b>CEDET</b> configuration process, and a wide array of
   customizations that helps tune your Emacs for programming the smart
   way.

<p>The image to the right shows the CEDET internal package and
   revision testing output.  Making sure all the package revisions
   needed are accurate is important, and fully automatic.

<br clear=all>

<h4>Contribute to CEDET</h4>

<p>In many ways, <b>CEDET</b> is a big pile of infrastructure with a
   thin user interface sitting on it.  Consider helping <b>CEDET</b>
   by using the infrastructure to build better or more user interface
   tools.  You can also help by <a href="addlang.shtml">adding more
   support for different languages</a>.  Join
   the <a href="http://lists.sourceforge.net/lists/listinfo/cedet-devel">cedet-devel</a>
   mailing list to learn more.

<br clear=all>
<p>

<?php
  include ("download.php")
?>
</p>

<?php
  include ("footer.fsf.shtml")
?>
