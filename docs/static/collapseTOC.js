var collapseTOC = () =>  {
  console.debug("collapsing TOC")
  document.querySelectorAll('#TOC > ul > li').forEach( function(li) {
    if (li.children.length > 1) {
      var toggle = document.createElement('span');
      toggle.className = 'toggle';
      toggle.innerHTML = '▸';
      toggle.onclick = function() {
        var sublist = li.getElementsByTagName('ul')[0];
        if (sublist) {
          if (sublist.style.display === 'none') {
            sublist.style.display = 'block'
            toggle.innerHTML = '▾';
          } else {
            sublist.style.display = 'none';
            toggle.innerHTML = '▸';
          }
        }
      };
      li.appendChild(toggle);
      toggle.click();
    }
  });
};

window.onload = collapseTOC;