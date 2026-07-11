// Force light mode so doxygen-awesome's dark-mode media query doesn't override
// our custom colour variables (HTML_COLORSTYLE=LIGHT doesn't add this class).
document.documentElement.classList.add("light-mode");

document.addEventListener("DOMContentLoaded", function () {
  // Inject a "back to docs" link into the existing doxygen-awesome project
  // header (#projectname or #titlearea) rather than adding a second toolbar.
  var target = document.getElementById("projectname")
             || document.getElementById("titlearea")
             || document.getElementById("top");
  if (!target) return;

  var link = document.createElement("span");
  link.id = "funwave-back-link";
  link.innerHTML = '<a href="../index.html">&#8592; Back to docs</a>';
  target.appendChild(link);

  // Enforce a minimum sidebar width so it doesn't collapse on window resize.
  // navtree.js sets sidenav.style.width and doc-content.style.marginLeft in
  // tandem; we watch for changes and clamp both to MIN_SIDEBAR_PX.
  var MIN_SIDEBAR_PX = 220;
  var sideNav   = document.getElementById("side-nav");
  var docContent = document.getElementById("doc-content");
  if (!sideNav || !docContent) return;

  var clamping = false;
  var observer = new MutationObserver(function () {
    if (clamping) return;
    var w = parseFloat(sideNav.style.width);
    if (!isNaN(w) && w < MIN_SIDEBAR_PX) {
      clamping = true;
      sideNav.style.width    = MIN_SIDEBAR_PX + "px";
      docContent.style.marginLeft = MIN_SIDEBAR_PX + "px";
      clamping = false;
    }
  });
  observer.observe(sideNav, { attributes: true, attributeFilter: ["style"] });
});
