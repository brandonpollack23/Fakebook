# Fakebook
Server:
The server consists of a Spray-Can server that makes use of a number of spray modules, including but not limited to Spray-JSON, Spray-Routing.
The structure of the server is as follows:
an F_Server actor acts as the binded listener to the server and services a connection by creating an F_Listener actor and Registering that to handle more connections
This way many connections can be serviced at a time.  The F_Listener actor handles the routing for the server and is passed the F_BackBone actor, who is made when the server is instantiated.
This BackBone actor has children for different services, namely the PageHandler, UserHandler, and the PicturesHandler.  They do what needs to be done and service requests, all of which are forwarded by backbone.

