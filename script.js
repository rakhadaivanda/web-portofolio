// Navbar scroll effect
const nav = document.querySelector('.navbar');
window.addEventListener('scroll', () => {
  nav.classList.toggle('solid', window.scrollY > window.innerHeight * 0.1);
});

// Portfolio filter
const filterBtns = document.querySelectorAll('.filter button');
const cards = document.querySelectorAll('.card');
filterBtns.forEach(btn => btn.addEventListener('click', () => {
  const cat = btn.dataset.cat;
  filterBtns.forEach(b=>b.classList.remove('active'));
  btn.classList.add('active');
  cards.forEach(c => {
    c.style.display = (cat === 'all' || c.dataset.cat === cat) ? 'block' : 'none';
  });
}));

// Audio button
const audio = document.getElementById('bg-audio');
const btn = document.getElementById('audio-btn');
btn.addEventListener('click', () => {
  if (audio.paused) {
    audio.play(); btn.textContent = '🔈';
  } else {
    audio.pause(); btn.textContent = '🔇';
  }
});
