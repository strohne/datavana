# Epiygraf Python Package

The Epygraf package makes working with Epigraf data from Python easier.

## Installation

You can install the package directly from GitHub:
```
pip install "git+https://github.com/strohne/datavana.git#egg=epygraf&subdirectory=epygraf"
```

Alternatively, clone the repository and from inside the epygraf folder call:
```
pip install -e .
```

The e option is useful when developing the package, you can change code without reinstalling the package. 
In Jupyter, after installing or changing the package, don't forget to restart the kernel.

## Usage

There are two ways to access Epigraf data: 

- **API**: Used to access data and create jobs from outside the server. 
- **Database**: Presumes you have a direct connection to the database server, e.g. in a development environment. 

## Database functions

Given you have direct access to an Epigraf server, 
you can show all article records from the epi_all-database 
by calling :

```
import epygraf as epi

epi.db.table("articles", "epi_all")
```


If your server runs under different settings than default, 
setup the connection settings first:

```
epi.db.setup(
    host="localhost",
	port=3306, 
	username="root", 
	password="root"
)
```

The table method returns a pandas dataframe, thus,
you can directly analyze the data:

```
articles = epi.db.table("articles")
articles.articletype.value_counts()
```


## API functions

*to be added*