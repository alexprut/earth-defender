/////////////////////////////////////////////////////
// Game Renderer and Logic
/////////////////////////////////////////////////////
var Game = function (config) {
    this.light = new Light(
        new THREE.Vector3(400, 0, 0),
        10000,
        new THREE.Vector3(0.09, 0.09, 0.1),
        new THREE.Vector3(0.4, 0.4, 0.4)
    );
    this.bullets = new THREE.Object3D;
    this.sun = null;
    this.earth = null;
    this.moon = null;
    this.scene = null;
    this.camera = null;
    this.renderer = null;
    this.spaceShip = [];
    this.meteorites = null;
    this.maxMeteorietes = config.maxMeteorietes || 200;
    this.controls = null;
    this.gui = null;
    this.stats = null;
    this.counter = {earthDamage: 0, collisionCheck: 0};
    this.maxLife = config.maxLife || 1000;
    this.life = this.maxLife;
    this.score = 0;
    this.requestAnimationFrameId = null;
    this.players = 0;
    this.maxPlayers = config.maxPlayers || 10;
    this.debug = config.debug || false;
    this.server = null;
    this.isMultiplayer = config.isMultiplayer || false;
    this.DOMHandler = new GameDOMHandler(this);
    this.roomId = null;

    if (this.isMultiplayer) {
        if (config.servers) {
            this.server = new GameClient({servers: config.servers, game: this});
        } else {
            throw new Error("A default server should be provided");
        }
    }

    if (this.debug) {
        this.createGui();
    }
};

var index = Math.floor((Math.random() * 1000) + 1);

Game.prototype.constructor = Game;
Game.prototype.createUniforms = function () {
    var loader = new THREE.TextureLoader();
    var uniforms = {
        lightPosition: {
            type: 'v3',
            value: this.light.lightPosition
        },
        ambient: {
            type: 'v3',
            value: this.light.ambientValue
        },
        rho: {
            type: "v3",
            value: this.light.rho
        },
        lightPower: {
            type: "f",
            value: this.light.lightPower
        },
        texture: {
            type: "t",
            value: loader.load('img/earth.jpg')
        }
    };

    uniforms.texture.value.minFilter = THREE.NearestMipMapNearestFilter;

    return uniforms;
};
Game.prototype.createVertexShader = function () {
    return document.getElementById("vertex").textContent;
};
Game.prototype.createFragmentShader = function () {
    return document.getElementById("fragment").textContent;
};
Game.prototype.setLife = function (life) {
    this.life = Math.max(life, 0);
    this.DOMHandler.setLife(this.life);
    if (this.life === 0) {
        this.stop("Game Over");
    }
};
Game.prototype.decreaseLife = function () {
    this.setLife(this.life - 200);
    if (this.server.websocket) {
        this.server.send("action_earth_collision");
    }
};
Game.prototype.increaseScore = function () {
    this.setScore(this.score + 10);
};
Game.prototype.setScore = function (score) {
    this.score = score;
    this.DOMHandler.setScore(this.score);
};
Game.prototype.createCamera = function () {
    var camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 2000);
    camera.position.x = 500;

    return camera;
};
Game.prototype.updatePlayers = function () {
    this.DOMHandler.setPlayers(this.players);
};
Game.prototype.setPlayers = function (players) {
    this.players = players;
    this.updatePlayers();
};
Game.prototype.setRoomsList = function (roomList) {
    this.DOMHandler.updateRoomsList(roomList);
};
Game.prototype.createSun = function () {
    var sun = new Sun();
    sun = sun.create();
    sun.position.x = 400;

    return sun;
};
Game.prototype.createEarth = function () {
    var earth = new Earth();
    earth = earth.create(
        this.createUniforms(),
        this.createVertexShader(),
        this.createFragmentShader()
    );
    earth.rotation.z = 10 * Math.PI / 180;

    return earth;
};
Game.prototype.createRender = function () {
    var renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setSize(window.innerWidth, window.innerHeight);

    return renderer;
};
Game.prototype.setRendererSize = function (width, height) {
    if (this.renderer) {
        this.renderer.setSize(width, height);
    }
};
Game.prototype.createSpaceShip = function () {
    var spaceShip = new SpaceShip();
    spaceShip[index] = spaceShip.create(
        this.createUniforms(),
        this.createVertexShader(),
        this.createFragmentShader()
    );
    spaceShip[index].position.x = Math.random() * 200 + 100;
    spaceShip[index].rotation.z = 90 * Math.PI / 180;

    return spaceShip[index];
};

Game.prototype.addNewSpaceShip = function (index) {
    for(var i = 0; i < index.length; i++){
        if(typeof this.spaceShip[index[i][0]] == "undefined")
            this.spaceShip[index[i][0]] = this.createSpaceShip();
            this.scene.add(this.spaceShip[index[i][0]]);
            }
};

Game.prototype.setPositionSpaceShip = function (position) {
    for(var index = 0; index < position.length; index++){
        this.spaceShip[position[index][0]].position.x = position[index][1];
        this.spaceShip[position[index][0]].position.y = position[index][2];
        this.spaceShip[position[index][0]].position.z = position[index][3];
        }
};

Game.prototype.createMoon = function () {
    var tmpMoon = new Moon();
    tmpMoon = tmpMoon.create(
        this.createUniforms(),
        this.createVertexShader(),
        this.createFragmentShader()
    );
    tmpMoon.position.x = 200;

    var moon = new THREE.Object3D();
    moon.add(tmpMoon);

    return moon;
};
Game.prototype.createMeteorites = function (numMeteorites) {
    var meteorites = new THREE.Object3D();
    for (var i = 0; i < numMeteorites; i++) {
        var meteorite = new Meteorite().create(
            this.createUniforms(),
            this.createVertexShader(),
            this.createFragmentShader()
        );
        meteorite.position.x = Math.random() * 1000 - Math.random() * 1000;
        meteorite.position.y = Math.random() * 1000 - Math.random() * 1000;
        meteorite.position.z = Math.random() * 1000 - Math.random() * 1000;
        meteorite.gameUUID = meteorite.uuid;

        meteorites.add(meteorite);
    }

    return meteorites;
};

Game.prototype.setAsteroidPosition = function (vector3) {
    for (var i = 0; i < vector3.length; i++) {
        this.meteorites.children[i].position.x = vector3[i][0];
        this.meteorites.children[i].position.y = vector3[i][1];
        this.meteorites.children[i].position.z = vector3[i][2];
    }
};


Game.prototype.createStats = function () {
    var stats = new Stats();
    stats.setMode(0);

    stats.domElement.id = "stats";

    document.body.appendChild(stats.domElement);

    return stats;
};
Game.prototype.createGui = function () {
    var gui = new dat.GUI();

    guiParams = {
        wireframe: true
    };

    var debugFolder = gui.addFolder('Debug');

    debugFolder.add(guiParams, 'wireframe').listen().onFinishChange((function () {
        this.sun.material.wireframe = guiParams.wireframe;
        this.earth.material.wireframe = guiParams.wireframe;
        this.spaceShip[index].material.wireframe = guiParams.wireframe;
        this.moon.children[0].material.wireframe = guiParams.wireframe;
        this.meteorites.children.forEach((function (meteorite) {
            meteorite.material.wireframe = guiParams.wireframe
        }).bind(this));
    }).bind(this));

    return gui;
};
Game.prototype.createControls = function () {
    var controls = new THREE.OrbitControls(this.camera);
    controls.maxDistance = 1000;
    controls.minDistance = 80;

    return controls;
};
Game.prototype.shoot = function () {
    var bullet = new Bullet().create();
    bullet.position.x = this.spaceShip[index].position.x - 10;
    bullet.position.y = this.spaceShip[index].position.y;
    bullet.position.z = this.spaceShip[index].position.z;

    this.bullets.add(bullet);

};

Game.prototype.shoot_online = function (index) {
    var bullet = new Bullet().create();
    bullet.position.x = this.spaceShip[index].position.x - 10;
    bullet.position.y = this.spaceShip[index].position.y;
    bullet.position.z = this.spaceShip[index].position.z;
    bullet.gameUUID = bullet.uuid;

    this.bullets.add(bullet);

};
Game.prototype.initEventMoveSpaceShip = function () {
    document.onkeydown = (function (e) {
        var key = e.keyCode ? e.keyCode : e.which;
        var data;
        if (key == 90) { //^
            this.spaceShip[index].position.y += 2;
            data = [index,'y+'];
        }
        if (key == 88) { //v
            this.spaceShip[index].position.y -= 2;
            data = [index,'y-'];
        }
        if (key == 83) { //v
            this.spaceShip[index].position.x += 2;
            data = [index,'x+'];
        }
        if (key == 87) { //^
            this.spaceShip[index].position.x -= 2;
            data = [index,'x-'];
        }
        if (key == 65) { //<
            this.spaceShip[index].position.z += 5;
            data = [index,'z+'];
        }
        if (key == 68) { //<
            this.spaceShip[index].position.z -= 5;
            data = [index,'z-'];
        }
        this.server.send("ship_move",data);
        data = [];
    }).bind(this);
};
Game.prototype.initEventShoot = function () {
    document.onkeypress = (function (e) {
        var key = e.keyCode ? e.keyCode : e.which;

        // Spacebar
        if (key == 32) {
            this.shoot();
            this.server.send("ship_shoot",index);
        }
    }).bind(this);
};

Game.prototype.checkCollision = function () {
    var meteoritesUUIDs = [], bulletsUUIDs = [], id;

    this.meteorites.children.forEach((function (meteorite) {
        var length = this.bullets.children.length;
        for (var i = 0; i < length; i++) {
            if (this.isCollision(meteorite.position, 5, this.bullets.children[i].position, 5)) {
                meteoritesUUIDs.push(meteorite.gameUUID);
                bulletsUUIDs.push(this.bullets.children[i].gameUUID);
                this.increaseScore();
                break;
            }
        }
    }).bind(this));

    while (id = meteoritesUUIDs.pop()) {
        var length = this.meteorites.children.length;
        for (var i = 0; i < length; i++) {
            if (id === this.meteorites.children[i].gameUUID) {
                this.meteorites.children.splice(i, 1);
                break;
            }
        }
    }

    while (id = bulletsUUIDs.pop()) {
        var length = this.bullets.children.length;
        for (var i = 0; i < length; i++) {
            if (id === this.bullets.children[i].gameUUID) {
                this.bullets.children.splice(i, 1);
                break;
            }
        }
    }
};
Game.prototype.isCollision = function (v1, l1, v2, l2) {
    // AABB (Axis-aligned bounding boxes)
    if (((v1.x - l1) <= (v2.x + l2) && (v1.x + l1) >= (v2.x - l2)) &&
        ((v1.y - l1) <= (v2.y + l2) && (v1.y + l1) >= (v2.y - l2)) &&
        ((v1.z - l1) <= (v2.z + l2) && (v1.z + l1) >= (v2.z - l2))) {
        return true;
    }

    return false;
};
Game.prototype.init = function () {
    this.renderer = this.createRender();
    this.scene = new THREE.Scene();
    this.camera = this.createCamera();
    this.stats = this.createStats();
    this.controls = this.createControls();
    this.sun = this.createSun();
    this.earth = this.createEarth();
    this.moon = this.createMoon();
    this.meteorites = this.createMeteorites(this.maxMeteorietes);
    this.spaceShip[index] = this.createSpaceShip();
    this.scene.add(this.sun);
    this.scene.add(this.moon);
    this.scene.add(this.earth);
    this.scene.add(this.bullets);
    this.scene.add(this.spaceShip[index]);
    this.scene.add(this.meteorites);

    this.initEventMoveSpaceShip();
    this.initEventShoot();

    this.DOMHandler.init();
    this.render();
};
Game.prototype.pause = function () {
    cancelAnimationFrame(this.requestAnimationFrameId);
};
Game.prototype.start = function () {
    if (this.requestAnimationFrameId) {
        this.render();
    } else {
        this.init();
    }
};
Game.prototype.stop = function (msg) {
    cancelAnimationFrame(this.requestAnimationFrameId);
    this.requestAnimationFrameId = null;
    this.DOMHandler.setMessage(msg);
};
Game.prototype.render = function () {
    this.requestAnimationFrameId = requestAnimationFrame(this.render.bind(this));

    this.camera.position.x = this.spaceShip[index].position.x + 45;
    this.camera.position.y = this.spaceShip[index].position.y + 10;
    this.camera.position.z = this.spaceShip[index].position.z;
    this.stats.begin();
    this.renderer.render(this.scene, this.camera);
    this.stats.end();

    // Rotate the sphere
    this.earth.rotation.y += 0.001;
    this.moon.rotation.y += 0.0005;

    this.bullets.children.forEach((function (bullet) {
        bullet.position.x -= 1;
    }).bind(this));

    this.counter.earthDamage--;
    if (this.counter.earthDamage === 0) {
        this.earth.material.uniforms.ambient.value = this.light.ambientValue;
    }

    this.counter.collisionCheck--;
    if (this.counter.collisionCheck <= 0) {
        this.checkCollision();
        this.counter.collisionCheck = 10;
    }

    this.meteorites.children.forEach((function (meteorite, index) {
        var moveFactor = 0.05;
        var x = meteorite.position.x;
        var y = meteorite.position.y;
        var z = meteorite.position.z;
        // Planets become Red near the earth
        if (-60 < x && x < 60 && -60 < y && y < 60 && -60 < z && z < 60) {
            this.meteorites.children[index].material.uniforms.ambient.value = new THREE.Vector3(0.9, 0.1, 0.1);
        }

        if (-38 < x && x < 38 && -38 < y && y < 38 && -38 < z && z < 38) {
            this.meteorites.children.splice(index, 1);
            this.counter.earthDamage = 30;
            this.earth.material.uniforms.ambient.value = new THREE.Vector3(0.9, 0.1, 0.1);
            this.decreaseLife();
        }

        if (x < 0) {
            meteorite.position.x += moveFactor;
        } else {
            meteorite.position.x -= moveFactor;
        }

        if (y < 0) {
            meteorite.position.y += moveFactor;
        } else {
            meteorite.position.y -= moveFactor;
        }

        if (z < 0) {
            meteorite.position.z += moveFactor;
        } else {
            meteorite.position.z -= moveFactor;
        }
    }).bind(this));
};
