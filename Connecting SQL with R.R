library(DBI)
# Connect to my SQL database : con
con <-dbConnect(RMySQL :: MySQL(),
                dbname = "ig_clone",
                host ="localhost",
                port=3306,
                user="root",
                password="mugendim")
## Get table names
tables<-dbListTables(con)
tables

# display structure of tables
str(tables)


# How to import tables
#Importing the comment table in my db
comment<-dbReadTable(con,"comments")
comment

#How to import all tables
# Get table names
tables<-dbListTables(con)

# Import all tables
tables<-lapply(tables,dbReadTable,conn=con)


#I,porting data from queries
my_table<-dbGetQuery(con,"SELECT COUNT(DISTINCT(users.id)) AS total_number_of_users_with_posts
FROM users
JOIN photos ON users.id = photos.user_id;
 ")
my_table


#Send Query to the database
myquery<-dbSendQuery(con,"SELECT COUNT(DISTINCT(users.id)) AS total_number_of_users_with_posts
FROM users
JOIN photos ON users.id = photos.user_id;
")
# Get the first 2 rows
dbFetch(myquery,n=2)

# get all rows
dbFetch(myquery,n=-1)
 #Clear my query
dbClearResult(myquery)


#Remove credentials from code

#connect to the mysql database:con
con<-dbConnect(RMySQL :: MySQL(),
               dbname = "ig_clone",
               host ="localhost",
               port=3306,
               user=rstudioapi::askForPassword("Database user"),
               password=rstudioapi::askForPassword("Database password"))



