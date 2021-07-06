// ==UserScript==
// @name        Github font changer
// @namespace   local.greasemonkey.githubfontchanger
// @include     https://github.com/*
// @version     1
// @grant       none
// ==/UserScript==

var fontdef = "Monospace ! important"; // Set your font here.

// Function helper to inject css
function addGlobalStyle(css) {
    var head, style;
    head = document.getElementsByTagName('head')[0];
    if (!head) return;

    style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = css;
    head.appendChild(style);
}

// Apply the font-family definition to code styles.
addGlobalStyle(
  '.blob-code { font-family: ' + fontdef + '; } ' +
  '.blob-num { font-family: ' + fontdef + '; } ' +
  '.text-mono { font-family: ' + fontdef + '; } ' +
  '');
