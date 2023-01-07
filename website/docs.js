// Mark the current section in the table of contents with aria-current when scrolled into view.
let tocLinks = document.querySelectorAll('.table-of-contents a');
let headers = new Map();
for (let link of tocLinks) {
  let headerId = link.hash.slice(1);
  let header = document.getElementById(headerId);
  headers.set(header, link);
}

let intersectingHeaders = new Set();
let observer = new IntersectionObserver(entries => {
  for (let entry of entries) {
    if (entry.isIntersecting) {
      intersectingHeaders.add(entry.target);
    } else {
      intersectingHeaders.delete(entry.target);
    }
  }

  if (intersectingHeaders.size > 0) {
    let current = document.querySelector('.table-of-contents a[aria-current]');
    if (current) {
      current.removeAttribute('aria-current');
    }
    let first;
    for (let [header, link] of headers) {
      if (intersectingHeaders.has(header)) {
        first = link;
        break;
      }
    }
    first.setAttribute('aria-current', 'location');
  }
});

for (let header of headers.keys()) {
  observer.observe(header);
}
