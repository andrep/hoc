TITLE = HOC: a Haskell to Objective-C bridge

NULL = 
SOURCE_PODS = Introduction.pod \
	      Mapping_Types.pod \
	      Accessing_Core_Frameworks.pod \
	      Accessing_Other_Frameworks.pod \
	      Creating_an_Objective-C_Class_in_Haskell.pod \
	      History.pod \
	      Appendices.pod \
	      $(NULL)

all: view

HOC.html: $(SOURCE_PODS)
	pod2html \
	  --index \
	  --header \
	  --title="${TITLE}" \
	  --outfile "$@" \
	  $+

view: HOC.html
	open $+

clean:
	-rm HOC.html pod2htm*

