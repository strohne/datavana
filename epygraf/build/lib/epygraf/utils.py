def drop_empty_columns(df):
    return df.dropna(axis=1, how='all')