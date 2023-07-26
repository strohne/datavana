# Epiygraf Python Package

The Epygraf package makes working with Epigraf data from Python easier.

## Installation
*to be added*

## Usage

There are two ways to acces Epigraf data: 

a) API: Used to access data and create jobs from outside the server
b) Direct database access: Presumes you have a direct connection to the database server, e.g. in a development environment

Example:
Given you have direct access to the servers, you can get alle article records of a database:

```
db_table("articles")
```
