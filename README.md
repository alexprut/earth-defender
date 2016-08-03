Earth Defender
--------------
A single/multiplayer 3D browser game build with Erlang and Three.js

__Live preview__: [https://alexprut.github.io/earth-defender/](https://alexprut.github.io/earth-defender/)

![Demo - Earth Defender](https://github.com/alexprut/earth-defender/raw/master/img/demo.png)


## Installation
Below you can see the instructions for building and starting the game _client_ and _server_.

#### Client
1. The client code is inside the ```./client``` directory, open your terminal and type:
    ```cd client```
2. Install [Bower](https://bower.io/) dependencies:
    ```bower install```
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
    ```docker run -p 8000:80 -v "$PWD"/client:/usr/share/nginx/html:ro nginx```
    You can now access the game at ```http://localhost:8000```

#### Server
1. The server code is inside the ```./server``` directory, open your terminal and type:
    ```cd server```
2. Install Erlang.mk (see official instructions [here](https://erlang.mk/guide/installation.html)):
    ```
        curl https://erlang.mk/erlang.mk -o erlang.mk
        make -f erlang.mk bootstrap
    ```
3. Install all dependencies and build the server application:
    ```make```
4. Run the server (the server should be listening on address and port ```localhost:8888```):
    ```make run```


Licence
-------
Project created by Alex Prut (Alexandru Pruteanu) and Boubakr Injarn, during the Master's Degree in Computer Science
at the University of Udine, for the _Interactive 3D Graphics_ and the _Distributed Systems_ classes.
