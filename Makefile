# This Makefile is only used to update the Examples.html file. use cabal to build and install the library

happstack-lite.html: Examples.lhs Makefile
	echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"" > $@
	echo "    \"http://www.w3.org/TR/html4/strict.dtd\">" >> $@
	echo "<html><head><title>happstack-lite tutorial</title><link rel='stylesheet' type='text/css' href='hscolour.css' ></head><body>" >> $@
	HsColour -lit -css -partial $< | markdown --html4tags >> $@
	echo "</body></html>">> $@
	validate $@
