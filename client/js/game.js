/////////////////////////////////////////////////////
// three.js Game Elements
/////////////////////////////////////////////////////

var Light = function (lightPosition, lightPower, ambientValue, rho) {
    this.lightPosition = lightPosition;
    this.lightPower = lightPower;
    this.ambientValue = ambientValue;
    this.rho = rho;
};
Light.prototype.constructor = Light;


var Element = function (texture) {
    this.texture = texture;
};
Element.prototype.createShaderMaterial = function (uniforms, vertexShader, fragmentShader) {
    uniforms.texture.value = new THREE.ImageUtils.loadTexture(this.texture);

    return new THREE.ShaderMaterial({
        uniforms: uniforms,
        vertexShader: vertexShader,
        fragmentShader: fragmentShader,
        wireframe: true
    });
};

var Earth = function () {
    Element.call(this, 'img/earth.jpg');
};
Earth.prototype = Object.create(Element.prototype);
Earth.prototype.constructor = Earth;
Earth.prototype.create = function (uniforms, vertexShader, fragmentShader) {
    return new THREE.Mesh(
        new THREE.SphereGeometry(40, 20, 20),
        this.createShaderMaterial(uniforms, vertexShader, fragmentShader)
    );
};


var Moon = function () {
    Element.call(this, 'img/moon.jpg');
};
Moon.prototype = Object.create(Element.prototype);
Moon.prototype.constructor = Moon;
Moon.prototype.create = function (uniforms, vertexShader, fragmentShader) {
    return new THREE.Mesh(
        new THREE.SphereGeometry(20, 15, 15),
        this.createShaderMaterial(uniforms, vertexShader, fragmentShader)
    );
};


var Sun = function () {
    Element.call(this, 'img/sun.jpg');
};
Sun.prototype = Object.create(Element.prototype);
Sun.prototype.constructor = Sun;
Sun.prototype.create = function () {
    return new THREE.Mesh(
        new THREE.SphereGeometry(20, 15, 15),
        new THREE.MeshBasicMaterial({
            map: THREE.ImageUtils.loadTexture(this.texture),
            wireframe: true
        })
    );
};


var SpaceShip = function () {
    Element.call(this, 'img/spaceship.jpg');
};
SpaceShip.prototype = Object.create(Element.prototype);
SpaceShip.prototype.constructor = SpaceShip;
SpaceShip.prototype.create = function (uniforms, vertexShader, fragmentShader) {
    return new THREE.Mesh(
        new THREE.CylinderGeometry(1, 5, 20, 4),
        this.createShaderMaterial(uniforms, vertexShader, fragmentShader)
    );
};

var Bullet = function () {
};
Bullet.prototype.constructor = Bullet;
Bullet.prototype.create = function () {
    return new THREE.Mesh(
        new THREE.BoxGeometry(5, 2, 2),
        new THREE.MeshBasicMaterial({color: 0xff0000})
    );
};


var Meteorite = function () {
    Element.call(this, 'img/meteorite.bmp');
};
Meteorite.prototype = Object.create(Element.prototype);
Meteorite.prototype.constructor = Meteorite;
Meteorite.prototype.create = function (uniforms, vertexShader, fragmentShader) {
    return new THREE.Mesh(
        new THREE.SphereGeometry(5, 5, 5),
        this.createShaderMaterial(uniforms, vertexShader, fragmentShader)
    );
};


/////////////////////////////////////////////////////
// Game DOM Handler
/////////////////////////////////////////////////////
Element.prototype.remove = function () {
    this.parentElement.removeChild(this);
};

var DOMHandler = function (gameHandler) {

    function setLife(life) {
        document.getElementById('life').innerHTML = life;
    }

    function setPlayers(players) {
        document.getElementById('players').innerHTML = players;
    }

    function setScore(score) {
        document.getElementById('score').innerHTML = score;
    }

    function init() {
        if (!gameHandler.isMultiplayer) {
            document.getElementById('gameType').className = 'hidden';
        } else {
            document.getElementById('gameType-singlePlayer').addEventListener('click', function () {
                console.log("Single Player game type selected");
                document.getElementById('gameType').className = 'hidden';
                gameHandler.stop("Loading ...");
                gameHandler.start();
            });

            document.getElementById('gameType-multiPlayer').addEventListener('click', function () {
                console.log("Multiplayer game type selected");
            });
        }

        while (document.getElementsByTagName('canvas').length) {
            document.getElementsByTagName('canvas')[0].remove();
        }

        document.body.appendChild(gameHandler.renderer.domElement);

        clearMessage();
        resetLife();
        resetScore();
    }

    function resetLife() {
        setLife(gameHandler.maxLife);
    }

    function resetScore() {
        setScore(0);
    }

    function setMessage(msg) {
        document.getElementById('message').className = '';
        document.getElementById('message').innerHTML = msg;
    }

    function clearMessage() {
        document.getElementById('message').className = 'hidden';
        document.getElementById('message').innerHTML = '';
    }

    return {
        init: init,
        setLife: setLife,
        setPlayers: setPlayers,
        setScore: setScore,
        resetLife: resetLife,
        resetScore: resetScore,
        setMessage: setMessage,
        clearMessage: clearMessage
    }
};

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
    this.spaceShip = null;
    this.meteorites = null;
    this.maxMeteorietes = config.maxMeteorietes || 200;
    this.controls = null;
    this.gui = null;
    this.stats = null;
    this.counter = 0;
    this.maxLife = config.maxLife || 1000;
    this.life = this.maxLife;
    this.score = 0;
    this.requestAnimationFrameId = null;
    this.players = 0;
    this.maxPlayers = config.maxPlayers || 10;
    this.debug = config.debug || false;
    this.server = null;
    this.isMultiplayer = config.isMultiplayer || false;
    this.DOMHandler = new DOMHandler(this);

    if (this.isMultiplayer) {
        if (config.servers) {
            this.server = new GameClient({servers: config.servers, game: this});
            this.server.connect();
        } else {
            throw new Error("A default server should be provided");
        }
    }

    if (this.debug) {
        this.initGui();
    }
};
Game.prototype.constructor = Game;
Game.prototype.createUniforms = function () {
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
            value: THREE.ImageUtils.loadTexture('img/earth.jpg')
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
Game.prototype.updateLife = function () {
    if (this.life > 0) {
        this.life -= 200;
        this.DOMHandler.setLife(this.life);
    } else {
        this.stop("Game Over");
    }
};
Game.prototype.initCamera = function () {
    var camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 2000);
    camera.position.z = 500;

    return camera;
};
Game.prototype.updatePlayers = function () {
    this.DOMHandler.setPlayers(this.players);
};
Game.prototype.setPlayers = function (players) {
    this.players = players;
    this.updatePlayers();
};
Game.prototype.initSun = function () {
    var sun = new Sun();
    sun = sun.create();
    sun.position.x = 400;

    return sun;
};
Game.prototype.initEarth = function () {
    var earth = new Earth();
    earth = earth.create(
        this.createUniforms(),
        this.createVertexShader(),
        this.createFragmentShader()
    );
    earth.rotation.z = 10 * Math.PI / 180;

    return earth;
};
Game.prototype.initRender = function () {
    var renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setSize(window.innerWidth, window.innerHeight);

    return renderer;
};
Game.prototype.initSpaceShip = function () {
    var spaceShip = new SpaceShip();
    spaceShip = spaceShip.create(
        this.createUniforms(),
        this.createVertexShader(),
        this.createFragmentShader()
    );
    spaceShip.position.x = Math.random() * 200 + 100;
    spaceShip.rotation.z = 90 * Math.PI / 180;

    return spaceShip;
};
Game.prototype.initMoon = function () {
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
Game.prototype.initMeteorites = function (numMeteorites) {
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

        meteorites.add(meteorite);
    }

    return meteorites;
};
Game.prototype.initStats = function () {
    var stats = new Stats();
    stats.setMode(0);

    document.body.appendChild(stats.domElement);

    return stats;
};
Game.prototype.initGui = function () {
    var gui = new dat.GUI();

    guiParams = {
        wireframe: true
    };

    var debugFolder = gui.addFolder('Debug');

    debugFolder.add(guiParams, 'wireframe').listen().onFinishChange((function () {
        this.sun.material.wireframe = guiParams.wireframe;
        this.earth.material.wireframe = guiParams.wireframe;
        this.spaceShip.material.wireframe = guiParams.wireframe;
        this.moon.children[0].material.wireframe = guiParams.wireframe;
        this.meteorites.children.forEach((function (meteorite) {
            meteorite.material.wireframe = guiParams.wireframe
        }).bind(this));
    }).bind(this));

    return gui;
};
Game.prototype.initControls = function () {
    var controls = new THREE.OrbitControls(this.camera);
    controls.maxDistance = 1000;
    controls.minDistance = 80;

    return controls;
};
Game.prototype.shoot = function () {
    var bullet = new Bullet().create();

    bullet.position.x = this.spaceShip.position.x - 10;
    bullet.position.y = this.spaceShip.position.y;
    bullet.position.z = this.spaceShip.position.z;

    this.bullets.add(bullet);

};
Game.prototype.initEventMoveSpaceShip = function () {
    document.onkeydown = (function (e) {
        var key = e.keyCode ? e.keyCode : e.which;

        if (key == 104) { //^
            this.spaceShip.position.y += 2;
        }
        if (key == 98) { //v
            this.spaceShip.position.y -= 2;
        }
        if (key == 102) { //>
            this.spaceShip.position.x += 2;
        }
        if (key == 100) { //<
            this.spaceShip.position.x -= 2;
        }
        if (key == 97) { //<
            this.spaceShip.position.z += 5;
        }
        if (key == 99) { //<
            this.spaceShip.position.z -= 5;
        }
    }).bind(this);
};
Game.prototype.initEventShoot = function () {
    document.onkeypress = (function (e) {
        var key = e.keyCode ? e.keyCode : e.which;

        // Spacebar
        if (key == 32) {
            this.shoot();
        }
    }).bind(this);
};
Game.prototype.init = function () {
    this.renderer = this.initRender();
    this.scene = new THREE.Scene();
    this.camera = this.initCamera();
    this.stats = this.initStats();
    this.controls = this.initControls();
    this.sun = this.initSun();
    this.earth = this.initEarth();
    this.moon = this.initMoon();
    this.spaceShip = this.initSpaceShip();
    this.meteorites = this.initMeteorites(this.maxMeteorietes);

    this.scene.add(this.sun);
    this.scene.add(this.moon);
    this.scene.add(this.earth);
    this.scene.add(this.bullets);
    this.scene.add(this.spaceShip);
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
    console.log(this.requestAnimationFrameId);
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

    this.camera.position.x = this.spaceShip.position.x + 15;
    this.camera.position.y = this.spaceShip.position.y;
    this.camera.position.z = this.spaceShip.position.z;
    this.stats.begin();
    this.renderer.render(this.scene, this.camera);
    this.stats.end();

    // Rotate the sphere
    this.earth.rotation.y += 0.001;
    this.moon.rotation.y += 0.0005;

    this.bullets.children.forEach((function (bullet) {
        bullet.position.x -= 1;
    }).bind(this));

    this.counter--;
    if (this.counter === 0) {
        this.earth.material.uniforms.ambient.value = this.light.ambientValue;
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
            this.counter = 30;
            this.earth.material.uniforms.ambient.value = new THREE.Vector3(0.9, 0.1, 0.1);
            this.updateLife();
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
