# How to Build OTP like documentation

## Utilities to prepare XML files

### Create XML files from code

If there are EDoc comments in a module, the escript `xml_from_edoc.escript` can be used to generate an XML file according to the `erlref` DTD for this module.

Example:

```text

	1> escript $ERL_TOP/lib/erl_docgen/priv/bin/xml_from_edoc.escript ex1.erl
```

### Include code in XML

If there are OTP DTD *codeinclude* tags in the source XML file, the escript `codeline_preprocessing.escript` can be used to include the code and produce an XML file according to the OTP DTDs.

Example:

```text

	1> escript $ERL_TOP/lib/erl_docgen/priv/bin/codeline_preprocessing.escript \
	   ex1.xmlsrc ex1.xml
```

## Use xsltproc to generate different output formats

### Parameters used in all the the XSL transformations

These parameters to `xsltproc` are used for all the supported output formats.

* __`docgen`__ - Path to erl_docgen's top directory.

* __`gendate`__ - The date string that will be used in the documentation.

* __`appname`__ - The name of the application.>

* __`appver`__ - The version of the application.

### Generate HTML output

When generating HTML one also needs these three pramaters to `xsltproc`.

* __`outdir`__ - Output directory for the produced html files.

* __`topdocdir`__ - If one builds a standalone documentation for an application this should be set to ".".

* __`pdfdir`__ - Relative path from the html directory to where the pdf file are placed.

Example:

```text
	
	1> xsltproc --noout --stringparam outdir /tmp/myhtmldoc \
	      --stringparam docgen $ERL_TOP/lib/erl_docgen \
              --stringparam topdocdir . \
              --stringparam pdfdir $PDFDIR \
              --xinclude \
	      --stringparam gendate "December 5 2011" \
              --stringparam appname MyApp \
              --stringparam appver 0.1 \
              -path $ERL_TOP/lib/erl_docgen/priv/dtd \
              -path $ERL_TOP/lib/erl_docgen/priv/dtd_html_entities \
	      $ERL_TOP/lib/erl_docgen/priv/xsl/db_html.xsl mybook.xml
```

### Generate PDF

The generation of the PDF file is done in two steps. First is `xsltproc` used to generate a `.fo` file which is used as input to the `fop` command to produce th PDF file.

Example:

```text

	1> xsltproc --output MyApp.fo \
             --stringparam docgen $ERL_TOP/lib/erl_docgen \
	     --stringparam gendate  "December 5 2011" \
             --stringparam appname MyApp \
             --stringparam appver 0.1 \
             --xinclude \
             -path $ERL_TOP/lib/erl_docgen/priv/dtd \
             -path $ERL_TOP/lib/erl_docgen/priv/dtd_html_entities \
	     $ERL_TOP/lib/erl_docgen/priv/xsl/db_pdf.xsl mybook.xml


        2> fop -fo MyApp.fo -pdf MyApp.pdf
```

### Generate man pages

Unix man pages can be generated with `xsltproc` from XML files written according to the different OTP ref type DTDs.

Example:

```text

	1> xsltproc --output  my_module.3\
	      --stringparam docgen $ERL_TOP/lib/erl_docgen \
	      --stringparam gendate  "December 5 2011" \
	      --stringparam appname MyApp \
	      --stringparam appver 0.1 \
	      --xinclude -path $ERL_TOP/lib/erl_docgen/priv/dtd  \
              -path $ERL_TOP/lib/erl_docgen/priv/dtd_man_entities \
              $ERL_TOP/lib/erl_docgen/priv/xsl/db_man.xsl my_refpage.xml
```

### Upcoming changes

The output from the `erl_docgen` documentation build process is now just the OTP style. But in a near future we will for example add the possibility to change logo, color in the PDF and style sheet for the HTML.
