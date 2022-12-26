// Mark the current section in the table of contents with aria-current when scrolled into view.
let tocLinks = document.querySelectorAll('.table-of-contents a');
let headers = [];
for (let link of tocLinks) {
  let headerId = link.hash.slice(1);
  let header = document.getElementById(headerId);
  headers.push({ link, header });
}

function updateToc() {
  for (let header of headers) {
    if ((header.header.offsetTop + header.header.getBoundingClientRect().height) > document.documentElement.scrollTop) {
      let currentSelection = document.querySelector('.table-of-contents a[aria-current]');
      if (currentSelection) {
        currentSelection.removeAttribute('aria-current');
      }
      header.link.setAttribute('aria-current', 'location');
      break;
    }
  }
}

updateToc();
document.addEventListener('scroll', () => {
  requestAnimationFrame(updateToc);
});
