/* Figure lightbox. Binds to every <img> inside a <figure> in .essay.
   Open by tap/click. Pinch to zoom on touch, wheel to zoom on desktop,
   tap/click to toggle between fit and 1:1 anchored at the point you touched,
   drag or swipe to pan, Esc or backdrop to close.
   No dependencies. Without JS the figures render inline exactly as before. */
(function () {
  "use strict";

  var triggers = document.querySelectorAll(".essay figure img");
  if (!triggers.length) return;

  var overlay, stage, big, cap, hint, closeBtn;
  var built = false, lastFocus = null;
  var natW = 0, natH = 0;
  var scale = 1, fitScale = 1, maxScale = 1, tx = 0, ty = 0;

  var pointers = {};
  var pointerCount = 0;
  var panFrom = null, pinchFrom = null;
  var moved = false;

  function clamp(v, lo, hi) { return v < lo ? lo : (v > hi ? hi : v); }
  function dist(a, b) { return Math.sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)); }
  function zoomable() { return maxScale > fitScale * 1.02; }

  function apply() {
    var sw = stage.clientWidth, sh = stage.clientHeight;
    var iw = natW * scale, ih = natH * scale;
    tx = iw <= sw ? (sw - iw) / 2 : clamp(tx, sw - iw, 0);
    ty = ih <= sh ? (sh - ih) / 2 : clamp(ty, sh - ih, 0);
    big.style.transform = "translate(" + tx + "px," + ty + "px) scale(" + scale + ")";
    overlay.classList.toggle("is-zoomed", scale > fitScale * 1.02);
  }

  function measure() {
    if (!natW || !natH) return;
    fitScale = Math.min(stage.clientWidth / natW, stage.clientHeight / natH, 1);
    maxScale = Math.max(fitScale, 1);
    big.style.width = natW + "px";
    big.style.height = natH + "px";
    overlay.classList.toggle("can-zoom", zoomable());
  }

  /* zoom while pinning the image point currently under (px,py) to that same screen point */
  function zoomTo(next, px, py) {
    next = clamp(next, fitScale, maxScale);
    var ix = (px - tx) / scale, iy = (py - ty) / scale;
    scale = next;
    tx = px - ix * scale;
    ty = py - iy * scale;
    apply();
  }

  function stagePoint(clientX, clientY) {
    var r = stage.getBoundingClientRect();
    return { x: clientX - r.left, y: clientY - r.top };
  }

  function onBigLoad() {
    natW = big.naturalWidth;
    natH = big.naturalHeight;
    measure();
    scale = fitScale;
    tx = 0; ty = 0;
    apply();
  }

  function build() {
    overlay = document.createElement("div");
    overlay.className = "lb-overlay";
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-modal", "true");
    overlay.setAttribute("aria-label", "Enlarged figure");
    overlay.hidden = true;
    overlay.innerHTML =
      '<button class="lb-close" type="button" aria-label="Close">\u00d7</button>' +
      '<div class="lb-stage"><img class="lb-img" alt="" draggable="false"></div>' +
      '<p class="lb-cap"></p>' +
      '<p class="lb-hint"></p>';
    document.body.appendChild(overlay);

    stage = overlay.querySelector(".lb-stage");
    big = overlay.querySelector(".lb-img");
    cap = overlay.querySelector(".lb-cap");
    hint = overlay.querySelector(".lb-hint");
    closeBtn = overlay.querySelector(".lb-close");

    hint.textContent = ("ontouchstart" in window)
      ? "Pinch, or tap the figure, to zoom"
      : "Click or scroll to zoom, drag to pan";

    big.addEventListener("load", onBigLoad);
    closeBtn.addEventListener("click", close);

    overlay.addEventListener("click", function (e) {
      if (e.target === overlay || e.target === cap || e.target === hint) close();
    });

    /* tap or click toggles fit <-> 1:1, anchored where you touched */
    stage.addEventListener("click", function (e) {
      if (moved) { moved = false; return; }
      if (!zoomable()) { close(); return; }
      var p = stagePoint(e.clientX, e.clientY);
      zoomTo(scale > fitScale * 1.02 ? fitScale : maxScale, p.x, p.y);
    });

    stage.addEventListener("pointerdown", function (e) {
      if (stage.setPointerCapture) stage.setPointerCapture(e.pointerId);
      if (!pointers[e.pointerId]) pointerCount++;
      pointers[e.pointerId] = stagePoint(e.clientX, e.clientY);
      moved = false;
      var ids = Object.keys(pointers);
      if (pointerCount === 1) {
        panFrom = { x: pointers[ids[0]].x, y: pointers[ids[0]].y, tx: tx, ty: ty };
        pinchFrom = null;
      } else if (pointerCount === 2) {
        var a = pointers[ids[0]], b = pointers[ids[1]];
        pinchFrom = {
          d: dist(a, b) || 1,
          mx: (a.x + b.x) / 2, my: (a.y + b.y) / 2,
          scale: scale, tx: tx, ty: ty
        };
        panFrom = null;
      }
    });

    stage.addEventListener("pointermove", function (e) {
      if (!pointers[e.pointerId]) return;
      pointers[e.pointerId] = stagePoint(e.clientX, e.clientY);
      if (e.cancelable) e.preventDefault();
      var ids = Object.keys(pointers);

      if (pointerCount >= 2 && pinchFrom) {
        var a = pointers[ids[0]], b = pointers[ids[1]];
        var d = dist(a, b) || 1;
        var mx = (a.x + b.x) / 2, my = (a.y + b.y) / 2;
        var ix = (pinchFrom.mx - pinchFrom.tx) / pinchFrom.scale;
        var iy = (pinchFrom.my - pinchFrom.ty) / pinchFrom.scale;
        scale = clamp(pinchFrom.scale * (d / pinchFrom.d), fitScale, maxScale);
        tx = mx - ix * scale;
        ty = my - iy * scale;
        moved = true;
        apply();
      } else if (panFrom) {
        var p = pointers[e.pointerId];
        var dx = p.x - panFrom.x, dy = p.y - panFrom.y;
        if (Math.abs(dx) > 4 || Math.abs(dy) > 4) moved = true;
        tx = panFrom.tx + dx;
        ty = panFrom.ty + dy;
        apply();
      }
    });

    function release(e) {
      if (pointers[e.pointerId]) { delete pointers[e.pointerId]; pointerCount--; }
      if (pointerCount < 2) pinchFrom = null;
      if (pointerCount === 1) {
        var id = Object.keys(pointers)[0];
        panFrom = { x: pointers[id].x, y: pointers[id].y, tx: tx, ty: ty };
      }
      if (pointerCount <= 0) { pointerCount = 0; pointers = {}; panFrom = null; }
    }
    stage.addEventListener("pointerup", release);
    stage.addEventListener("pointercancel", release);

    stage.addEventListener("wheel", function (e) {
      if (!zoomable()) return;
      e.preventDefault();
      var p = stagePoint(e.clientX, e.clientY);
      zoomTo(scale * (e.deltaY < 0 ? 1.18 : 1 / 1.18), p.x, p.y);
    }, { passive: false });

    window.addEventListener("resize", function () {
      if (overlay.hidden) return;
      var wasFit = scale <= fitScale * 1.02;
      measure();
      if (wasFit) { scale = fitScale; tx = 0; ty = 0; }
      else scale = clamp(scale, fitScale, maxScale);
      apply();
    });

    document.addEventListener("keydown", function (e) {
      if (overlay.hidden) return;
      if (e.key === "Escape") { close(); return; }
      if ((e.key === "Enter" || e.key === " ") && document.activeElement !== closeBtn) {
        e.preventDefault();
        zoomTo(scale > fitScale * 1.02 ? fitScale : maxScale,
               stage.clientWidth / 2, stage.clientHeight / 2);
      }
    });

    built = true;
  }

  function open(img) {
    if (!built) build();
    lastFocus = document.activeElement;

    var fig = img.parentNode;
    while (fig && fig.tagName !== "FIGURE") fig = fig.parentNode;
    var fc = fig ? fig.querySelector("figcaption") : null;

    big.alt = img.alt || "";
    cap.textContent = fc ? fc.textContent.replace(/\s+/g, " ").trim() : "";
    cap.hidden = !cap.textContent;

    overlay.hidden = false;
    document.documentElement.classList.add("lb-open");
    big.src = img.currentSrc || img.src;
    if (big.complete && big.naturalWidth) onBigLoad();
    closeBtn.focus();
  }

  function close() {
    if (!built) return;
    overlay.hidden = true;
    document.documentElement.classList.remove("lb-open");
    pointers = {}; pointerCount = 0; panFrom = null; pinchFrom = null;
    big.removeAttribute("src");
    if (lastFocus && lastFocus.focus) lastFocus.focus();
  }

  Array.prototype.forEach.call(triggers, function (img) {
    img.classList.add("lb-trigger");
    img.setAttribute("tabindex", "0");
    img.setAttribute("role", "button");
    img.setAttribute("aria-label", "Enlarge figure");
    img.addEventListener("click", function () { open(img); });
    img.addEventListener("keydown", function (e) {
      if (e.key === "Enter" || e.key === " ") { e.preventDefault(); open(img); }
    });
  });
})();
