from setuptools import setup, find_packages

VERSION = '0.0.1' 
DESCRIPTION = 'Epygraf - analyse epigraf data with python'
LONG_DESCRIPTION = 'The Epygraf package aims to make data work with Epigraf easier. It provides functions for data transfer using the Epigraf APIs: preparing data imports, e.g. from social media datasets, and preparing data analyses with Python.'

# Setting up
setup(
       # the name must match the folder name 'epygraf'
        name="epygraf", 
        version=VERSION,
        author="Jakob Jünger und Chantal Gärtner",
        author_email="<youremail@email.com>",
        description=DESCRIPTION,
        long_description=LONG_DESCRIPTION,
        packages=find_packages(),
        install_requires=[], # add any additional packages that 
        # needs to be installed along with your package. Eg: 'caer'
        
        keywords=['python', 'first package'],
        classifiers= [
            "Development Status :: 3 - Alpha",
            "Intended Audience :: Education",
            "Programming Language :: Python :: 2",
            "Programming Language :: Python :: 3",
            "Operating System :: MacOS :: MacOS X",
            "Operating System :: Microsoft :: Windows",
        ]
)