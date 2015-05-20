var Earth = function () {
};
Earth.prototype.constructor = Earth;
Earth.prototype.create = function () {
    var geometry = new THREE.SphereGeometry(40, 20, 20);
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};


var Moon = function () {
};
Moon.prototype.constructor = Moon;
Moon.prototype.create = function () {
    var geometry = new THREE.SphereGeometry(20, 15, 15);
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};


var SpaceShip = function () {
};
SpaceShip.prototype.constructor = SpaceShip;
SpaceShip.prototype.create = function () {
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});
    var geometry = new THREE.CylinderGeometry(1, 5, 20, 4);

    return new THREE.Mesh(geometry, material);
};


var Meteorite = function () {
};
Meteorite.prototype.constructor = Meteorite;
Meteorite.prototype.create = function () {
    var geometry = new THREE.SphereGeometry(5, 10, 10);
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};


var Game = function () {
    this.earth = null;
    this.moon = null;
    this.scene = null;
    this.camera = null;
    this.renderer = null;
    this.spaceShip = null;
    this.meteorites = null;
    this.controls = null;
    this.gui = null;
    this.stats = null;
};
Game.prototype.constructor = Game;
Game.prototype.initCamera = function () {
    var camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.z = 500;

    return camera;
};
Game.prototype.initEarth = function () {
    var earth = new Earth();
    earth = earth.create();
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
    spaceShip = spaceShip.create();
    spaceShip.position.x = 100;
    spaceShip.rotation.z = 90 * Math.PI / 180;

    return spaceShip;
};
Game.prototype.initMoon = function () {
    var tmpMoon = new Moon();
    tmpMoon = tmpMoon.create();
    tmpMoon.position.x = 200;

    var moon = new THREE.Object3D();
    moon.add(tmpMoon);

    return moon;
};
Game.prototype.initMeteorites = function (numMeteorites) {
    var meteorites = new THREE.Object3D();
    for (var i = 0; i < numMeteorites; i++) {
        var meteorite = new Meteorite().create();
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
        wireframe: false
    };

    var debugFolder = gui.addFolder('Debug');

    debugFolder.add(guiParams, 'wireframe').listen().onFinishChange((function () {
        this.earth.material.wireframe = guiParams.wireframe;
        this.spaceShip.material.wireframe = guiParams.wireframe;
        this.moon.children[0].material.wireframe = guiParams.wireframe;
    }).bind(this));

    return gui;
};
Game.prototype.init = function () {
    this.renderer = this.initRender();
    this.scene = new THREE.Scene();
    this.camera = this.initCamera();
    this.stats = this.initStats();
    this.controls = new THREE.OrbitControls(this.camera);
    this.earth = this.initEarth();
    this.moon = this.initMoon();
    this.spaceShip = this.initSpaceShip();
    this.meteorites = this.initMeteorites(5);

    this.scene.add(this.moon);
    this.scene.add(this.earth);
    this.scene.add(this.spaceShip);
    this.scene.add(this.meteorites);

    this.initGui();

    document.body.appendChild(this.renderer.domElement);
    this.render();
};
Game.prototype.render = function () {
    requestAnimationFrame(this.render.bind(this));

    this.stats.begin();
    this.renderer.render(this.scene, this.camera);
    this.stats.end();

    // Rotate the sphere
    this.earth.rotation.y += 0.001;
    this.moon.rotation.y += 0.0005;

    this.meteorites.children.forEach((function (currentValue, index) {
        var moveFactor = 1;
        var meteorite = currentValue;
        var x = meteorite.position.x;
        var y = meteorite.position.y;
        var z = meteorite.position.z;

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

var game = new Game();
game.init();
