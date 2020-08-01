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

Similarly, you can use yarn for installation: `yarn install onwebed`.

## Getting Started

To create a website using Onwebed, you have to create *Onwebed Document(s) (OD)*, which defines your webpages, and are compiled to HTML files. Onwebed documents are defined using a markup language, called *Onwebed Document Language (ODL)*.

In this section, we're going to be creating a classic "Hello World" webpage. Begin with choosing a directory where you want to place your document (Onwebed Document) files.

You can create the document files (extension: ".od") manually, or you can use the web interface. It's recommended to use the web interface for beginners, as it's easier to use and has a flatter learning curve.

## Usage

### Overview


```
onwebed [-v|--version] [-s|--server] [-p|--port PORT] [-d|--dest DESTINATION_PATH] [SOURCE_PATH]
```

Available options:

| Option  | Details |
| ------------- | ------------- |
| -v, --version  | Display version of Onwebed. |
| -s, --server  | Start a server which hosts the visual editor. |
| -p, --port `PORT`  | Port which the server will use. |
| -d, --dest `DESTINATION_PATH` | Directory to save compiled files. |
| `SOURCE_PATH` | Directory where your documents reside. |
| -h, --help | Show this help text |

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

`<port>` specifies the port at which you want the server to listen to. It defaults to *3000*.

Example:

```bash
onwebed . -s
```

This will start the web interface server, with `.` as the source path.