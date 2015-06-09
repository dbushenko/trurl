# Overview

Trurl is a haskell scaffolding tool named after great hyper-space engineer-constructor Trurl who (with his friend Clapaucius) is the hero of Stanislaw Lem's fiction.

![Trurl](https://raw.githubusercontent.com/dbushenko/trurl/master/img/trurl.jpg "Trurl")

The tool is able to create template projects of any type and template files of any type. Though it was intended to generate template haskell code, you may also generate html/css/js in web projects and any other text files.

## Installation

Build it from source using cabal:

    git clone https://github.com/dbushenko/trurl.git
    cd trurl
    cabal install

Or install it from the repository:

    cabal install trurl

## Usage

Just run 'trurl' to see the help:

    trurl <command> [parameters]
      update -- fetch the updates from repository
      create <project_template> <name> -- create project of specified type with specified name
      new <template> <parameters_string> -- create file from the template with specified parameters, wrap it with ""
      list -- print all available templates
      help <template> -- print template info
      help -- print this help

* Command 'update' fetches all the latest templates from the repository. Run it before using any other command.
* Command 'list' shows available templates. Technically it finds all the files *.metainfo in $HOME/.trurl/repo and prints thir first lines.
* Command 'help <template>' prints detailed info about the template.
* Command 'create' intended to generate projects, just specify an available project template name.
* Command 'new' generates template file. It uses Mustache format in templates and accepts parameters in JSON format.

For example, if there is a template file 'file1.txt' with following contents:

```
The list: 

{{#heroes}}
    * {{name}}
{{/heroes}}

MyObj:
{{#myobj}}
{{opt1}}
{{/myobj}}
```

Run trurl as following:

    trurl new file1.txt '{"heroes":[{"name":"1"},{"name":"22"}],"myobj":{"opt1":"value1"}}'

Then you'll get:

```
The list: 

    * 1
    * 22

MyObj:
value1
```

## Creating templates

All the templates are stored in $HOME/.trurl/repo. There are two types of templates: projects and files.

* Project template is just a tar archive which is unpacked in specified directory.
* File template is a file in Mustache format. It may have any extension, but if not supplied while running 'trurl new ...' then extension '.hs' will be used.


Info about each template is stored in corresponding metainfo file. E.g. if there is template 'file1.hs' then should be also 'file1.hs.metainfo'. First line of the metainfo file is its short description printed by the 'list' command.

See examples of the templates in 'devrepo'.

## Registering templates in the main repository.

Just add a pull request in the directory 'devrepo'. You are welcome to add your own templates here!

Author: Dmitry Bushenko (d.bushenko@gmail.com)