var scene,
    camera,
    renderer,
    earth,
    moon,
    spaceShip,
    controls,
    guiParams,
    stats,
    gui;

var createEarth = function () {
    var geometry = new THREE.SphereGeometry(40, 20, 20);
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};

var createMoon = function () {
    var geometry = new THREE.SphereGeometry(20, 15, 15);
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};

var createSpaceShip = function () {
    var material = new THREE.MeshBasicMaterial({color: 0x0000ff});
    var geometry = new THREE.CylinderGeometry(1, 5, 20, 4);

    return new THREE.Mesh(geometry, material);
};

var init = function () {
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);

    renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);

    earth = createEarth();
    spaceShip = createSpaceShip();
    var tmpMoon = createMoon();

    tmpMoon.position.x = 200;
    moon = new THREE.Object3D();

    moon.add(tmpMoon);

    scene.add(moon);
    scene.add(earth);
    scene.add(spaceShip);

    controls = new THREE.OrbitControls(camera);

    spaceShip.position.x = 100;
    spaceShip.rotation.z = 90 * Math.PI / 180;

    camera.position.z = 500;

    createGui();
    initStats();

    render();
};

var createGui = function () {
    gui = new dat.GUI();

    guiParams = {
        wireframe: false
    };

    var debugFolder = gui.addFolder('Debug');

    debugFolder.add(guiParams, 'wireframe').listen().onFinishChange(function () {
        earth.material.wireframe = guiParams.wireframe;
        spaceShip.material.wireframe = guiParams.wireframe;
        moon.children[0].material.wireframe = guiParams.wireframe;
    });
};

var initStats = function () {
    stats = new Stats();
    stats.setMode(0);

    stats.domElement.style.position = 'absolute';
    stats.domElement.style.left = '0px';
    stats.domElement.style.top = '0px';

    document.body.appendChild(stats.domElement);
};

var render = function () {
    requestAnimationFrame(render);

    stats.begin();
    renderer.render(scene, camera);
    stats.end();
    // Rotate the sphere
    earth.rotation.y += 0.001;
    moon.rotation.y += 0.0005;
};

init();
