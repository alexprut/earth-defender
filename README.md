Earth Defender
--------------
A single/multiplayer 3D browser game build with Erlang and Three.js  
__Live preview__: [https://alexprut.github.io/earth-defender/](https://alexprut.github.io/earth-defender)

![Demo - Earth Defender](https://github.com/alexprut/earth-defender/raw/master/img/demo.png)

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

## Architecture
To allow multiplayer mode play the WebSocket technology is used, a protocol over HTTP for asynchronous communication that can occur concurrently.
Since browser can't receive a WebSocket connection, but only initialize one, client to client (P2P) connections is not possibile
due to the limitation of the protocol (to solve that problem WebRTC can be used).

### General Architecture
![General Architecture](https://github.com/alexprut/earth-defender/raw/real-time-multiplayer/documentation/General-Architecture.png)

Above in the image is illustrated the general architecture of multiplayer game. The client entities (or players) are just
browsers with support to WebSockets and WebGL. At first the clients (browser) make a request to the DNS in order to get the address of the WebServer, the DNS is also responsible for the Load Balancing. Once the DNS responses with the address to one of the WebServers the client
fetches all the assets (js, images, css, ...). The WebServer is only responsible for providing the game assets, not less nor more.
As last step the client contact one of the servers to join a game room or create one and then play. The servers are written in Erlang language
and intended to be fault tolerant and distributed.

### Use Cases
Below are listed the most relevant use cases, user interfaces and the
flow diagrams of the messages exchanged between the client and server.

#### Multiplayer, create new room
Below you can see the user interface:

![UI, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/real-time-multiplayer/documentation/UI-Use-Case-Multiplayer-create-Room.png)

Below you can see the message exchanged between the client and the server:

![Flow Diagram, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/real-time-multiplayer/documentation/Flow-Diagram-Use-Case-Multiplayer-create-Room.png)

#### Multiplayer, join a room
Below you can see the user interface:

![UI, Multiplayer, join new room](https://github.com/alexprut/earth-defender/raw/real-time-multiplayer/documentation/UI-Use-Case-Multiplayer-join-Room.png)

Below you can see the message exchanged between the client and the server:

![Flow Diagram, Multiplayer, create new room](https://github.com/alexprut/earth-defender/raw/real-time-multiplayer/documentation/Flow-Diagram-Use-Case-Multiplayer-join-Room.png)

## Server
The server is written in Erlang language, the libraries used are:

*  [Cowboy](https://github.com/ninenines/cowboy): which provides support for HTTP and WebSockets
*  [Jiffy](https://github.com/davisp/jiffy): a library for handling JSON as external data representation

Licence
-------
Licensed under the MIT License – see the LICENSE file for details.

Project created by __Alexandru Pruteanu__ and __Boubakr Injarn__, during the Master's Degree in Computer Science
at the University of Udine, for the _Interactive 3D Graphics_ and the _Distributed Systems_ classes.
