var scene,
    camera,
    renderer,
    earth,
    spaceShip,
    controls;

var createEarth = function () {
    var geometry = new THREE.SphereGeometry(40, 20, 20);
    var material = new THREE.MeshBasicMaterial({wireframe: true, color: 0x0000ff});

    return new THREE.Mesh(geometry, material);
};

var createSpaceShip = function () {
    var material = new THREE.MeshBasicMaterial({wireframe: true, color: 0x0000ff});
    var geometry = new THREE.CylinderGeometry( 1, 5, 20, 4);

    return new THREE.Mesh(geometry, material);
};

var init = function () {
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);

    renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);

    earth = createEarth();
    spaceShip = createSpaceShip();
    scene.add(earth);
    scene.add(spaceShip);

    controls = new THREE.OrbitControls(camera);

    spaceShip.position.x = 100;
    spaceShip.rotation.z = 90 * Math.PI / 180;

    camera.position.z = 500;

    render();
};

var render = function () {
    requestAnimationFrame(render);

    renderer.render(scene, camera);

    // Rotate the sphere
    earth.rotation.y += 0.001;
};

init();
