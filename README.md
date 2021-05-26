# Onwebed

A simple and easy-to-use [web template system](https://en.wikipedia.org/wiki/Web_template_system), designed for crafting static sites.

## Features

- **Markup Language**. Templating is done through a markup language.
- **Web Interface**. Onwebed ships with a web interface, which houses a visual editor for the markup language, and includes features for managing documents. It's essentially a web app hosted locally.
- **CLI Interface**.

## About

Onwebed has been primarily created to handle the structure and content of a webpage, independent of each other. As with any template system, it minimizes redundancy.

Onwebed is based on a markup language (based on XML); for which there is a compiler, and a visual editor.

The markup language has been designed with clean and simple visual editing in mind, avoiding the necessity of WYSIWYG editors.

## Installation

Onwebed is bundled as a node.js package. You install it through npm: `npm install onwebed`.

Similarly, you can use yarn for installation: `yarn add onwebed`.

## Getting Started

To create a website using Onwebed, you have to create _Onwebed Document(s) (OD)_, which defines your webpages, and are compiled to HTML files. Onwebed documents are defined using a markup language, called _Onwebed Document Language (ODL)_.

### Hello World

In this section, we're going to be creating a classic "Hello World" webpage. Begin with choosing a directory where you want to place your document (Onwebed Document) files.

#### Creating a New Document

You can create the document files (extension: ".od") manually, or you can use the web interface. It's recommended to use the web interface for beginners, as it's easier to use and has a flatter learning curve.

Open the URL which the server starts in (will be stated through the CLI) in your browser. You'll be greeted with the list of documents. Go ahead and create a new document by clicking the `+ New` button, providing the name of the document, and submitting the form.

#### Opening up the Document

Open up the document in the visual editor by clicking the `Edit` button for the respective document.

#### Defining the Structure

To create a "Hello World" webpage with proper HTML document, we need a structure such a paragraph (p) element inside a body element, which in turn is inside an html element. The paragraph element will be for holding the text "Hello World".

To create the structure, we need to add _bones_ to our document. Bones are defined with a _descriptor_. We'll learn more about it later, but right now let's add a new bone to the document through: `+ Element` > `Bone`. A new bone will be added to the document, which for a while will be bordered green. Let's set its descriptor to "html body p", which will render to an HTML structure of `html` > `body` > `p`.

#### Defining the Content

Now that we defined the structure, we need to add the textual content. This is defined by _flesh_. Let's create a new flesh: `+ Element` > `Flesh`. Just as we saw with the bone, a new item will be added to the document, which will be bordered green for a while; this represents the flesh. Set the content (multi-line text field) of the flesh to "Hello World", as we planned.

#### Linking the Content with the Structure

To place the text (defined by the flesh item) inside the paragraph (p) element (defined by the bone), we need to add an identifier to the paragraph element. Update the bone descriptor to "html body p@hello-world". Notice the addition of "@hello-world". This simply adds the identifier `hello-world` to `p`.

Now that `p` got an identifier, the flesh item can refer to it, or target it, and place the textual content inside it. To do so, update the "Targets" of the flesh item to "hello-world"; this will make the flesh item target the paragraph element. And with that done, we got a connection between the structure and the content we just created. It's time to check out the results!

#### Saving and Viewing

Before viewing the rendered webpage, we need to save the document by clicking the `Save` button. Afterwards, clicking the `View` button which will open the rendered webpage in a new tab of your browser. You should see the text "Hello World"!

## Usage

### Overview

```
onwebed [-v|--version] [-s|--server] [-p|--port PORT] [-d|--dest DESTINATION_PATH] [SOURCE_PATH]
```

Available options:

| Option                        | Details                                       |
| ----------------------------- | --------------------------------------------- |
| -v, --version                 | Display version of Onwebed.                   |
| -s, --server                  | Start a server which hosts the visual editor. |
| -p, --port `PORT`             | Port which the server will use.               |
| -d, --dest `DESTINATION_PATH` | Directory to save compiled files.             |
| `SOURCE_PATH`                 | Directory where your documents reside.        |
| -h, --help                    | Show this help text                           |

### Compiling Documents

```bash
onwebed <path>
```

`<path>` represents the location (relative) of the directory where your documents reside; also known as the source path. You can omit it to refer to the current directory.

You can specify the path to the destination directory using the `-d` option:

Example:

```bash
onwebed test/documents
```

Here, Onwebed will compile all the documents, which reside inside the relative path "test/documents".

### Starting the Web Interface

```bash
onwebed <path> -s
```

`<path>` represents the location (relative) of your document files. You can omit it to refer to the current directory.

`-s` is for starting the server, which hosts the web app.

You can explicitly specify the port at which the server will listen to (8000, in this case) using the `-p` option:

```bash
onwebed <path> -s -p <port>
```

`<port>` specifies the port at which you want the server to listen to. It defaults to _3000_.

Example:

```bash
onwebed . -s
```

This will start the web interface server, with `.` as the source path.
