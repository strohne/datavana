def db_connect(db="epi_all"):
    user = 'root'
    pw = 'root'
    host = 'localhost'
    port = 3306

    db_str = 'mysql+pymysql://' + \
             user + ':' + \
             pw + '@' + \
             host + ':' + \
             str(port) + '/' + \
             db

    db_con = create_engine(db_str)

    return (db_con)

def db_unconnect(db_con):
    db_con.dispose()

def db_table(table="", db="epi_all"):

    # connect to database
    db_con = db_connect(db)

    # Get table
    # filter deleted = 0
    db_table = pd.read_sql(
        'SELECT * FROM ' + table + ' WHERE deleted = 0',
        con=db_con
    )

    # close database connection
    db_unconnect(db_con)

    return(db_table)
