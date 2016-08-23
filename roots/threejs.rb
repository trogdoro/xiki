class Threejs
  STANDALONE = %`
        | <style>canvas{ width: 100%; height: 100% } body{margin:0px}</style>
        | <body>
        |   <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r71/three.js"></script>
        |   <script>
        |     // Standard setup
        |     var scene = new THREE.Scene();
        |     var camera = new THREE.PerspectiveCamera(60, window.innerWidth/window.innerHeight, 0.1, 1000);
        |     var renderer = new THREE.WebGLRenderer({antialias:true});
        |     renderer.setSize(window.innerWidth, window.innerHeight);
        |     document.body.appendChild(renderer.domElement);
        |     var geometry = new THREE.CubeGeometry(1, 1, 3);
        |     var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
        |     var cube = new THREE.Mesh(geometry, material);
        |     scene.add(cube);
        |     camera.position.z = 6;
        |
        |     // Add spotlight
        |     var light = new THREE.SpotLight(0xffffff, 1.0, 100.0, true);
        |     light.position.set(2, 1, 10);
        |     scene.add(light);
        |
        |     // Make it spin
        |     function render() {
        |       requestAnimationFrame(render);
        |       cube.rotation.y -= 0.01;
        |       renderer.render(scene, camera);
        |     }
        |     render();
        |   </script>
        | </body>`

  MENU = %`
    - geometry/
      - standalone/#{STANDALONE}
      - shapes/
        - cube/
          | var material = new THREE.MeshLambertMaterial({color: 0x6666ff});
          | var cube2 = new THREE.Mesh(geometry, material);
          | cube2.position.set(-1, -1, -1);
          | scene.add(cube2);
        - block/
          | var material = new THREE.MeshLambertMaterial({color: 0x6666ff});
          | var bar = new THREE.Mesh(THREE.CubeGeometry(1, 1, 3), material);
          | scene.add(bar);
          | function render() {   // Make it spin
          |   requestAnimationFrame(render);  bar.rotation.y -= 0.002;  renderer.render(scene, camera);
          | }
        - add randomly/
          | var color = new THREE.Color()
          | color.setRGB(Math.random(), Math.random(), Math.random())
          | var material = new THREE.MeshLambertMaterial({color: color});
          | var cube2 = new THREE.Mesh(geometry, material);
          | cube2.position.set(Math.floor(Math.random()*5)-2, Math.floor(Math.random()*5)-2, Math.floor(Math.random()*5)-2);
          | scene.add(cube2);
        - cylinder/
          | geometry = new THREE.CylinderGeometry(0.5, 0.5, 1, 100);
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(-1, 1, -1);
          | scene.add(shape);
        - cone/
          | geometry = new THREE.CylinderGeometry(0.0, 0.7, 1, 100);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(1, 0, 1);
          | scene.add(shape);
        - lamp shade/
          | geometry = new THREE.CylinderGeometry(0.5, 1, 1, 100);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(1, -1, -1);
          | scene.add(shape);
        - sphere/
          | geometry = new THREE.SphereGeometry(0.5, 20, 20);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(-1, -1, 1);
          | scene.add(shape);
        - icosahedron/
          | geometry = new THREE.IcosahedronGeometry(0.5);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(-1, 1, 1);
          | scene.add(shape);
        - octahedron/
          | geometry = new THREE.OctahedronGeometry(1);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(1, -1, 1);
          | scene.add(shape);
        - tetrahedron/
          | geometry = new THREE.TetrahedronGeometry(1);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(1, 1, 1);
          | scene.add(shape);
        - text/
          | geometry = new THREE.TextGeometry("hey you", size:10);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(2, 1, 1);
          | scene.add(shape);
        - torus/
          | geometry = new THREE.TorusGeometry(0.5, 0.3);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(1, 2, 1);
          | scene.add(shape);
        - torus knot/
          | geometry = new THREE.TorusKnotGeometry(0.5, 0.3);
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(-2, 2, 1);
          | scene.add(shape);
        - arrow helper/
          | geometry = new THREE.ArrowHelperGeometry();
          | var material = new THREE.MeshLambertMaterial({color: 0x88ff88});
          | var shape = new THREE.Mesh(geometry, material);
          | shape.position.set(-2, 2, 1);
          | scene.add(shape);
      - light/
        - intensity/
          - dark/
            | light.intensity = 0.0;
          - dim/
            | light.intensity = 0.3;
          - bright/
            | light.intensity = 1.0;
        - position/
          - front/
            | light.position.set(0, 0, 10);
          - back/
            | light.position.set(3, 3, -10);
          - side/
            | light.position.set(10, 0, 2);
        - color/
          - red/
            | light.color = new THREE.Color(0xff3333)
          - blue/
            | light.color = new THREE.Color(0x3333ff)
          - yellow/
            | light.color = new THREE.Color(0xffff00)
          - white/
            | light.color = new THREE.Color(0xffffff)
      - rotation/
        - none/
          | function render() {
          |   requestAnimationFrame(render);
          |   renderer.render(scene, camera);
          | }
        - one axis/
          | function render() {
          |   requestAnimationFrame(render);
          |   cube.rotation.y += 0.01;
          |   renderer.render(scene, camera);
          | }
        - three axes/
          | function render() {
          |   requestAnimationFrame(render);
          |   cube.rotation.x += 0.01;
          |   cube.rotation.y += 0.005;
          |   cube.rotation.z += 0.008;
          |   renderer.render(scene, camera);
          | }
      - position/
        | cube.position.set(1,1,0);
      - camera/
        - near/
          | camera.position.z = 3;
        - medium/
          | camera.position.z = 5;
        - far/
          | camera.position.z = 15;
        - offset/
          | camera.position.set(2,2,10);
      - mouse dragging/
        > Doesn't work yet
        | controls = new THREE.TrackballControls(camera);
        | function render() {
        |   requestAnimationFrame(render);
        |   renderer.render(scene, camera);
        |   controls.update();
        | }
      - more complicated/
        | <body></body>
        |
        | <script src="http://stemkoski.github.com/Three.js/js/Three.js"></script>
        | <script src="http://stemkoski.github.com/Three.js/js/Detector.js"></script>
        | <script src="http://stemkoski.github.com/Three.js/js/Stats.js"></script>
        | <script src="http://stemkoski.github.com/Three.js/js/THREEx.KeyboardState.js"></script>
        | <script src="http://stemkoski.github.com/Three.js/js/THREEx.FullScreen.js"></script>
        | <script src="http://stemkoski.github.com/Three.js/js/THREEx.WindowResize.js"></script>
        |
        | <script>
        |
        | // MAIN
        | if ( ! Detector.webgl ) Detector.addGetWebGLMessage();
        | // standard global variables
        | var container, scene, camera, renderer, controls, stats;
        | var keyboard = new THREEx.KeyboardState();
        | var clock = new THREE.Clock();
        | // custom global variables
        | var cube;
        | // Color of cube
        | var cubeMaterial = new THREE.MeshLambertMaterial( { color: 0x880000 } );
        |
        |
        | init();
        | animate();
        |
        | function init() {
        |
        |   // Try changing these:
        |
        |   // Scene
        |   scene = new THREE.Scene();
        |
        |   // Camera
        |   var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
        |   var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;
        |   camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);
        |   scene.add(camera);
        |   camera.position.set(0,150,400);
        |   camera.lookAt(scene.position);
        |   // Renderer
        |   renderer = new THREE.WebGLRenderer( {antialias:true} );
        |   renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
        |   container = document.createElement( 'div' );
        |   document.body.appendChild( container );
        |   container.appendChild( renderer.domElement );
        |   // Events
        |   THREEx.WindowResize(renderer, camera);
        |   THREEx.FullScreen.bindKey({ charCode : 'm'.charCodeAt(0) });
        |   // Controls
        |   controls = new THREE.TrackballControls( camera );
        |   // Stats
        |   stats = new Stats();
        |   stats.domElement.style.position = 'absolute';
        |   stats.domElement.style.bottom = '0px';
        |   stats.domElement.style.zIndex = 100;
        |   container.appendChild( stats.domElement );
        |
        |   // Light
        |   var light = new THREE.PointLight(0xffffff);
        |   light.position.set(100,250,150);
        |   scene.add(light);
        |   var light = new THREE.PointLight(0xffffff);
        |   light.position.set(-100,-250,-150);
        |   scene.add(light);
        |
        |   /////////
        |   // Box //
        |   /////////
        |   var cubeGeometry = new THREE.CubeGeometry( 50, 50, 50 );
        |   cube = new THREE.Mesh( cubeGeometry, cubeMaterial );
        |   cube.position.set(0,26,0);
        |   scene.add(cube);
        | }
        |
        | function animate()
        | {
        |   requestAnimationFrame( animate );
        |   render();
        |   update();
        | }
        |
        | function update()
        | {
        |   if ( keyboard.pressed("z") )
        |   {
        |     // do something
        |   }
        |
        |   controls.update();
        |   stats.update();
        | }
        |
        | function render()
        | {
        |   renderer.render( scene, camera );
        | }
        |
        | </script>
        |
        | </body>
        | </html>
        |
        | <!--
        |   Attribution:
        |   http://stemkoski.github.com/Three.js/Template.html
        |   -->
    - shaders/
      - hello world/
        | void main( void ) {
        |   // Just "one" color
        |   gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
        | }
      - gradient/
        | #ifdef GL_ES
        | precision highp float;
        | #endif
        |
        | uniform vec2 resolution;
        |
        | void main( void ) {
        |   vec3 colorA = vec3(1.0, 1.0, 1.0);
        |   vec3 colorB = vec3(0.0, 0.0, 0.0);
        |   float position = ( gl_FragCoord.y / resolution.y );
        |   vec3 color = mix(colorA, colorB, position);
        |   gl_FragColor = vec4(color, 1.0);
        | }
      - pulse/
        | #ifdef GL_ES
        | precision highp float;
        | #endif
        |
        | uniform float time;
        | uniform vec2 mouse;
        | uniform vec2 resolution;
        |
        | void main( void ) {
        |   vec2 position = ( gl_FragCoord.xy / resolution.xy ) + mouse / 4.0;
        |
        |   float color = 0.0;
        |   color += sin( position.x * cos( time / 15.0 ) * 80.0 ) + cos( position.y * cos( time / 15.0 ) * 10.0 );
        |   color += sin( position.y * sin( time / 10.0 ) * 40.0 ) + cos( position.x * sin( time / 25.0 ) * 40.0 );
        |   color += sin( position.x * sin( time / 5.0 ) * 10.0 ) + sin( position.y * sin( time / 35.0 ) * 80.0 );
        |   color *= sin( time / 10.0 ) * 0.5;
        |
        |   gl_FragColor = vec4( vec3( color, color * 0.5, sin( color + time / 3.0 ) * 0.75 ), 1.0 );
        | }
    - links/
      - api/
        @http://mrdoob.github.com/three.js/docs/55/
      - shaders/
        @http://mrdoob.com/projects/glsl_sandbox/
    - docs/
      - rectangle/
      - adding to a real projects/
        - see video/
    `

  def self.menu_after output, *path
    # Grab code if there...

    if output =~ /^\|/   # If expanded code
      txt = output.gsub /^\| ?/, ''
    elsif path[-1] =~ /^\|/ && path[-1] !~ /\n/   # If is code
      return "=beg/quoted/"
      # txt = ENV['txt'].dup
    end

    if path[-1] =~ /\n/
      txt = path[-1]
    end

    return if ! txt

    # If threejs/geometry/..., render as html

    if path[0] == "geometry"
      self.geometry path, txt
    else
      self.shaders txt
    end
    nil
  end

  def self.geometry path, txt
    # Run as js if no tags
    is_html = txt =~ /\A\s*</
    if is_html
      return Xiki::Browser.html(txt, :name=>"threejs")
    end

Ol["TODO: If not yet showing a canvas, open it first!"]
    location = Browser.js("String(window.location)")

    if location !~ /\/tmp\/threejs.html$/
Ol "STANDALONE", STANDALONE
      html = Tree.unquote(STANDALONE)
      html.sub! "scene.add(cube);", ''
      Xiki::Browser.html(html, :name=>"threejs")
Ol "location", location
    end

    # TODO: If not yet showing a canvas, open it first!
    Browser.js(txt)

    return
  end

  def self.shaders txt
    # Must be geometry/shaders/...

    self.prepare_browser { Browser.url "http://mrdoob.com/projects/glsl_sandbox/" }

    # If code, show in browser...

    txt = txt.inspect
    #     sleep 1
    Browser.js "
        $('#code').val(#{txt});
        compile();
      "
  end

  # Go to site and wait, if not already open
  def self.prepare_browser
    return if Browser.js('$("#code").length') == "1"

    yield

    max_checks = 20
    while (max_checks -= 1) > 0 && Browser.js('$("#code").length') != "1"
      sleep 0.2
    end

  end
end
