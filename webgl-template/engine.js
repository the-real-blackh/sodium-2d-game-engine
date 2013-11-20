var canvas = document.getElementById("mycanvas");
var gl;
var pos;
var pMatrixUniform;
var mvMatrixUniform;
var pMatrix = mat4.create();
var mvMatrix = mat4.create();
var samplerUniform;
var vShader;
var fShader;
var program;
var texCoordAttr;
var texCoords;
var vpWidth;
var vpHeight;
var loading = true;
var realFrame = null;
var loadingT0 = new Date().getTime();
var outstandingImages = 0;

function resizeViewport(width, height)
{
    canvas.width = width;
    canvas.height = height;
    vpWidth = width;
    vpHeight = height;
    gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
}

function getNumericStyleProperty(style, prop){
    return parseInt(style.getPropertyValue(prop),10) ;
}

function element_position(e) {
    var x = 0, y = 0;
    var inner = true ;
    do {
        x += e.offsetLeft;
        y += e.offsetTop;
        var style = getComputedStyle(e,null) ;
        var borderTop = getNumericStyleProperty(style,"border-top-width") ;
        var borderLeft = getNumericStyleProperty(style,"border-left-width") ;
        y += borderTop ;
        x += borderLeft ;
        if (inner){
          var paddingTop = getNumericStyleProperty(style,"padding-top") ;
          var paddingLeft = getNumericStyleProperty(style,"padding-left") ;
          y += paddingTop ;
          x += paddingLeft ;
        }
        inner = false ;
    } while (e = e.offsetParent);
    return { x: x, y: y };
}

function initGL(handleMouseDown, handleMouseUp, handleMouseMove) {
    canvas.onmousedown = function(event) {
        var offsetpos = element_position(canvas);
        handleMouseDown(event.pageX-offsetpos.x, event.pageY-offsetpos.y); }
    document.onmouseup = function(event) {
        var offsetpos = element_position(canvas);
        handleMouseUp(event.pageX-offsetpos.x, event.pageY-offsetpos.y); }
    document.onmousemove = function(event) {
        var offsetpos = element_position(canvas);
        handleMouseMove(event.pageX-offsetpos.x, event.pageY-offsetpos.y); }
    
    return gl;
}

function loadImage(fn, bg)
{
    if (outstandingImages == 0)
        loadingT0 = new Date().getTime();
    outstandingImages++;
    var tex = gl.createTexture();
    tex.loaded = false;
    tex.deleted = false;
    tex.image = new Image();
    if (bg) {
        tex.image.onload = function() {
            if (!tex.deleted) {
                tex.imgWidth = tex.image.width;
                tex.imgHeight = tex.image.height;
                var newCanvas = document.createElement("canvas");
                newCanvas.width = 1024;
                newCanvas.height = 1024;
                var ctx = newCanvas.getContext("2d");
                ctx.drawImage(tex.image, 0, 0, newCanvas.width, newCanvas.height);
                tex.image = new Image();
                // here is the most important part because if you dont replace you will get a DOM 18 exception.
                tex.image.src = newCanvas.toDataURL("image/png").replace("image/png", "image/octet-stream");
                tex.image.onload = function () { outstandingImages--; handleLoadedTexture(gl,tex); }
            }
        };
    }
    else {
        tex.image.onload = function() { outstandingImages--; if (!tex.deleted) handleLoadedTexture(gl,tex) };
    }
    tex.image.src = fn;
    return tex;
}

function handleLoadedTexture(gl,tex) {
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, tex.image);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.bindTexture(gl.TEXTURE_2D, null);
    tex.loaded = true;
}

function prepareImage()
{
    gl.bindBuffer(gl.ARRAY_BUFFER, texCoords);
    gl.vertexAttribPointer(texCoordAttr, texCoords.itemSize, gl.FLOAT, false, 0, 0);

    gl.activeTexture(gl.TEXTURE0);

    gl.bindBuffer(gl.ARRAY_BUFFER, squareBuf);
    gl.enableVertexAttribArray(pos);
    gl.vertexAttribPointer(pos, squareBuf.itemSize, gl.FLOAT, false, 0, 0);
}

function startRendering()
{
    gl.clearColor(1, 1, 1, 1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    mat4.identity(pMatrix);
    mat4.scale(pMatrix, pMatrix, [0.001 * vpHeight / vpWidth, 0.001, 1]);
    gl.uniformMatrix4fv(pMatrixUniform, false, pMatrix);

    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.enable(gl.BLEND);

    prepareImage();
}

function endRendering()
{
    if (loading || outstandingImages > 0) {
        var dt = new Date().getTime() - loadingT0
        if (dt >= 500)
            drawImage(loadingTex, 0, 0, 100, 100, false, dt/400);
    }
}

function requestAnimFrame2(f)
{
    if (loading) {
        realFrame = f;
        loading = false;
    }
    else
        requestAnimFrame(f);
}

function drawImage(tex, x, y, w, h, bg, rot)
{
    if (tex.loaded) {
        if (bg) {
            x = 0;
            y = 0;
            var scrnAspect = vpWidth / vpHeight;
            var imgAspect = tex.imgWidth / tex.imgHeight;
            if (imgAspect > scrnAspect) {
                w = 1000 * imgAspect;
                h = 1000;
            }
            else {
                w = 1000 * scrnAspect;
                h = w / imgAspect;
            }
        }

        mat4.identity(mvMatrix);
        mat4.translate(mvMatrix, mvMatrix, [x, y, 0]);
        if (rot != 0)
            mat4.rotate(mvMatrix, mvMatrix, rot, [0, 0, -1]);
        mat4.scale(mvMatrix, mvMatrix, [w, h, 1]);

        gl.bindTexture(gl.TEXTURE_2D, tex);
        gl.uniform1i(samplerUniform, 0);
        gl.uniformMatrix4fv(mvMatrixUniform, false, mvMatrix);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, squareBuf.numItems);
    }
}

function destroyImage(tex)
{
    tex.deleted = true;
    gl.deleteTexture(tex);
}

function getWindowWidth()
{
  if (typeof (window.innerWidth) == 'number')
    return window.innerWidth;
  else {
    if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight))
      return document.documentElement.clientWidth;
    else {
      if (document.body && (document.body.clientWidth || document.body.clientHeight))
        return document.body.clientWidth;
      else
        alert("no window width");
    }
  }
}

function getWindowHeight()
{
  if (typeof (window.innerHeight) == 'number')
    return window.innerHeight;
  else {
    if (document.documentElement && (document.documentElement.clientHeight || document.documentElement.clientHeight))
      return document.documentElement.clientHeight;
    else {
      if (document.body && (document.body.clientHeight || document.body.clientHeight))
        return document.body.clientHeight;
      else
        alert("no window width");
    }
  }
}

try {
    gl = canvas.getContext("webgl");
}
catch (x) { gl = null; }
if (gl == null) {
    try {
        gl = canvas.getContext("experimental-webgl");
    }
    catch (x) { gl = null; }
}
if (!gl) {
    alert("Could not initialise WebGL, sorry :-(");
}

vShader = createShaderFromScriptElement(gl, "shader-vs");
fShader = createShaderFromScriptElement(gl, "shader-fs");
var program = createProgram(gl, [vShader, fShader]);
gl.useProgram(program);

pos = gl.getAttribLocation(program, "aVertexPosition");
pMatrixUniform = gl.getUniformLocation(program, "uPMatrix");
mvMatrixUniform = gl.getUniformLocation(program, "uMVMatrix");

resizeViewport(canvas.offsetWidth, canvas.offsetHeight);

texCoordAttr = gl.getAttribLocation(program, "aTextureCoord");
gl.enableVertexAttribArray(texCoordAttr);
samplerUniform = gl.getUniformLocation(program, "uSampler");

texCoords = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, texCoords);
var textureCoords = [
  // Front face
  0.0, 0.0,
  1.0, 0.0,
  0.0, 1.0,
  1.0, 1.0
];
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(textureCoords), gl.STATIC_DRAW);
texCoords.itemSize = 2;
texCoords.numItems = 4;

squareBuf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, squareBuf);
var vertices = [
    -1.0,  1.0,  0.0,
     1.0,  1.0,  0.0,
    -1.0, -1.0,  0.0,
     1.0, -1.0,  0.0
];
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
squareBuf.itemSize = 3;
squareBuf.numItems = 4;

var loadingTex = loadImage("loading.png", false);
function drawWhileLoading() {
    if (!loading) { realFrame(); realFrame = null; } else {
        startRendering();
        endRendering();
        requestAnimFrame(drawWhileLoading);
    }
};
requestAnimFrame(drawWhileLoading);
