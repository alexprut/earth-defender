Earth Defender
--------------
A distributed soft real-time 3D single/multiplayer game build with Erlang/OTP and Three.js  
__Live preview__: [https://alexprut.github.io/earth-defender/](https://alexprut.github.io/earth-defender)

![Demo - Earth Defender](https://github.com/alexprut/earth-defender/raw/master/documentation/img/EarthDefenderMultiplayer.gif)


## Table of contents
1. [Installation](#installation)
    1. [Client](#client)
    2. [Server](#client)
2. [Architecture](#architecture)
    1. [General Architecture](#general-architecture)
    2. [Use Cases](#use-cases)
        1. [Multiplayer, create new room](#multiplayer-create-new-room)
        2. [Multiplayer, join a room](#multiplayer-join-a-room)
    4. [Client](#client-1)
    3. [Server](#server-1)
3. [License](#license)
    


## Installation
Below you can see the instructions for building and starting the game _client_ and _server_.

### Client
1. The client code is inside the ```./client``` directory, open your terminal and type:

    ```
    cd client
    ```

2. Install [Bower](https://bower.io/) dependencies:

    ```
    bower install
    ```

3. Setup the game client (by default in ```./client/js/main.js``` is included a default game initialization, the file is also included as default in ```./client/index.html```), below you can see how to initialize a game client:

    ```javascript
    document.body.onload = function () {
        game = new Game({
            'maxLife': 1000,
            'maxMeteorietes': 200,
            'isMultiplayer': true,
            'maxPlayers': 10,
            'debug': true
        });
        game.init();
    };
    ```

4. Now you should have a HTTP server to serve the game assets (html, css, ...), the tool to use is up to you (apache, nginx or whatever). As a quick example, you could install Docker and run the below code:

    ```
    docker run -p 8000:80 -v "$PWD"/client:/usr/share/nginx/html:ro nginx
    ```

    You can now access the game at ```http://localhost:8000```

### Server
1. The server code is inside the ```./server``` directory, open your terminal and type:

    ```
    cd server
    ```

2. Install Erlang.mk (see official instructions [here](https://erlang.mk/guide/installation.html)):

    ```
    curl https://erlang.mk/erlang.mk -o erlang.mk
    make -f erlang.mk bootstrap
    ```

3. Install all dependencies and build the server application:

    ```
    make
    ```

4. Run the server (the server should be listening on address and port ```localhost:8888```):

    ```
    make run
    ```

5. You can also run a slave/replica for fault-tolerant purposes (inside the ```./server/Makefile``` you can find a default configuration that you may change):

    ```
    make slave
    ```

## Architecture
To allow multiplayer mode play the WebSocket protocol is used, that runs over HTTP for asynchronous communication that can occur concurrently.
Since browser can't receive a WebSocket connection, but only initialize one, client to client (P2P) connections is not possibile
due to the limitation of the protocol (to solve that problem WebRTC can be used).

### General Architecture
![General Architecture](https://github.com/alexprut/earth-defender/raw/master/documentation/img/GeneralArchitecture.png)

Above in the image is illustrated the general architecture of multiplayer game. The client entities (or players) are just browsers with support to WebSockets and WebGL. At first the clients (browser) make a request to the DNS in order to get the address of the WebServer, the DNS is also responsible for the Load Balancing. Once the DNS responses with the address to one of the WebServers the client fetches all the assets (js, images, css, ...). The WebServer is only responsible for providing the game assets, not less nor more.
As last step the client contact one of the servers to join a game room or create one and then play. The servers are written in Erlang language and intended to be fault tolerant and distributed.

![CAP Theorem](https://github.com/alexprut/earth-defender/raw/master/documentation/img/CAP.png)

In a perfect world it would be nice to have both Availability and Consistency at the same time.
In a real world it's impossible and that is what the CAP Theorem say, you can only have 2 things at once (i.e. CP, AP, AC),
theoretically an AC solution could be possible, but we all know that there is no network and hardware that do not fail.
For the above game was opted for an AP (i.e. Availability and Partition tolerance) approach. In a soft real-time game it's more
important to have an available system than consistent (at least for this game). In case of failure or conflict it is preferred to lose
consistency (sync/solve somehow later) and gain availability.

### Use Cases
Below are listed the most relevant use cases, user interfaces and the flow diagrams of the messages exchanged between the client and server.

#### Multiplayer, create new room
Below you can see the user interface:

![UI, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/master/documentation/img/MultiplayerCreateRoomUC.png)

Below you can see the message exchanged between the client and the server:

![Flow Diagram, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/master/documentation/img/MultiplayerCreateRoom.png)

#### Multiplayer, join a room
Below you can see the user interface:

![UI, Multiplayer, join new room](https://github.com/alexprut/earth-defender/raw/master/documentation/img/MultiplayerJoinRoomUC.png)

Below you can see the message exchanged between the client and the server:

![Flow Diagram, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/master/documentation/img/MultiplayerJoinRoom.png)

### Client
The client is written using raw CSS, HTML & JavaScript.
Here the word client is an abuse, since it is intended as the game renderer/logic itself and the software used to communicate with the server in case of multiplayer game mode.
The only library used for the 3D rendering is Three.js.

Below you can see the client architecture:

![Client Architecture](https://github.com/alexprut/earth-defender/raw/master/documentation/img/ClientArchitecture.png)

The ```Game``` class is the core of the game, it contains the game renderer and logic, it uses various instances of the ```GameElements``` class which contains game basic objects.
Accordingly the classes ```GameMultiplayer``` and ```GameSingleplayer``` extends the basic game logic and add the features for the multiplayer and singleplayer game mode.
In case a multiplayer game mode is selected the ```GameMultiplayer``` class uses the ```GameClient``` class in order to be able to have a handler and a WebSocket connection with one of the servers.
Finally the ```GameDOMHandler``` class is responsible of the DOM manipulation activities, it is the class that moves things inside the browser.

### Server
The server is written in Erlang language, the libraries used are:

*  [Cowboy](https://github.com/ninenines/cowboy): which provides support for HTTP and WebSockets
*  [Jiffy](https://github.com/davisp/jiffy): a library for handling JSON as external data representation

Below you can see the client architecture:

![Server Architecture](https://github.com/alexprut/earth-defender/raw/master/documentation/img/ServerArchitecture.png)

The server is intended to be fault-tolerant oriented. As mentioned before the Cowboy
module is used to handle the WebSocket connection and the initial HTTP handshake of the
last mentioned protocol. The file ```earth_defender_app.erl``` is responsible for spawning
the one new server process (i.e. Cowboy) which will be the same for any request, ```earth_defender_sup.erl```
is the supervisor (i.e. if Cowboy crashes it restarts the process).
The core file that handles the WebSocket requests is ```websocket_handler.erl```, a fresh new instance
is made at every new client connection. The ```config.hrl``` file contains all the configuration,
and there is the place where the server is configured with various options.
Noteworthy is that the ```local_room_state.erl``` is spawned and initialized at the bootstraping
of the Cowboy application, the last mentioned file is a local state, that is where all the
information about room, players and other stuff is kept and shared among all the clients.
Finally the ```room.erl``` file spawns a new process every time a new room is created, and it is destroyed
when no more players are inside the room, obviously the ```player.erl``` file spawns a new process
for every client, and contains the information of the player. Finally the ```slave_handler.erl``` file is responsible for creating a
 consistent replica of the master and in case of a master crash taking over and becoming the new master server.


License
-------
Licensed under the MIT License – see the LICENSE file for details.
