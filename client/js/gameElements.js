/////////////////////////////////////////////////////
// three.js Game Elements
/////////////////////////////////////////////////////

// https://github.com/mrdoob/three.js/issues/9824
var TextureLoader = (function () {
    var _instance = null;

    var Loader = function () {
        var _loader = new THREE.TextureLoader();
        var _cache = [];

        function _cachePush(elem, val) {
            _cache.push({
                element: elem,
                value: val
            });
        }

        function _cacheSearch(elem) {
            for (var i = 0; i < _cache.length; i++) {
                if (_cache[i].element === elem) {
                    return _cache[i].value;
                }
            }

            return false;
        }

        function load(texture) {
            var match = _cacheSearch(texture);

            if (match) {
                return match;
            }

            var val = _loader.load(texture);
            _cachePush(texture, val);

            return val;
        }

        return {
            load: load
        }
    };

    function getInstance() {
        return (_instance) ? _instance : _instance = Loader();
    }

    return {
        getInstance: getInstance
    }
})();

var Light = function (lightPosition, lightPower, ambientValue, rho) {
    this.lightPosition = lightPosition;
    this.lightPower = lightPower;
    this.ambientValue = ambientValue;
    this.rho = rho;
};
Light.prototype.constructor = Light;


var Element = function (texture) {
    this.texture = texture;
    this.loader = TextureLoader.getInstance();
};
Element.prototype.createShaderMaterial = function (uniforms, vertexShader, fragmentShader) {
    uniforms.texture.value = this.loader.load(this.texture);

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
            map: this.loader.load(this.texture),
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
