

[apq_provider]
log_level="debug"

[apq_provider.engines.1]
engine="mysql"
slots="30"
host_name="localhost"
user="root"
password="senharoot"
db_name="@_lower_project_name_@_db"


# Notice you can also create other engines. This will give you the power of easily load balance your DB backends and also
# help the migration from one backend to another.
#
# Well, kinda.. the data inserted into the engine.1 wont be avaliable to other engines.. but someday we will fix it.. promisse!

