// ============================================
// PORTFOLIO — SCRIPT.JS
// Rakha Dayvanda Putra
// ============================================

// --- PRELOADER ---
window.addEventListener('load', () => {
  const preloader = document.getElementById('preloader');
  setTimeout(() => {
    preloader.classList.add('hidden');
  }, 600);
});

// --- NAVBAR SCROLL EFFECT ---
const navbar = document.getElementById('navbar');
window.addEventListener('scroll', () => {
  navbar.classList.toggle('scrolled', window.scrollY > 50);
});

// --- HAMBURGER MENU ---
const hamburger = document.getElementById('hamburger');
const navLinks = document.getElementById('nav-links');

hamburger.addEventListener('click', () => {
  hamburger.classList.toggle('active');
  navLinks.classList.toggle('active');
});

// Close mobile menu when a link is clicked
navLinks.querySelectorAll('a').forEach(link => {
  link.addEventListener('click', () => {
    hamburger.classList.remove('active');
    navLinks.classList.remove('active');
  });
});

// Close mobile menu on outside click
document.addEventListener('click', (e) => {
  if (!navbar.contains(e.target)) {
    hamburger.classList.remove('active');
    navLinks.classList.remove('active');
  }
});

// --- SMOOTH SCROLL FOR ANCHOR LINKS ---
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
  anchor.addEventListener('click', function (e) {
    const targetId = this.getAttribute('href');
    if (targetId === '#') return;
    e.preventDefault();
    const target = document.querySelector(targetId);
    if (target) {
      const navHeight = navbar.offsetHeight;
      const targetPosition = target.getBoundingClientRect().top + window.pageYOffset - navHeight;
      window.scrollTo({ top: targetPosition, behavior: 'smooth' });
    }
  });
});

// --- SCROLL REVEAL (Intersection Observer) ---
const revealElements = document.querySelectorAll('.reveal');

const revealObserver = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      entry.target.classList.add('visible');
      revealObserver.unobserve(entry.target);
    }
  });
}, {
  threshold: 0.1,
  rootMargin: '0px 0px -50px 0px'
});

revealElements.forEach(el => revealObserver.observe(el));

// --- ACTIVE NAV LINK ON SCROLL ---
const sections = document.querySelectorAll('section[id]');

const activateNavLink = () => {
  const scrollY = window.pageYOffset;
  sections.forEach(section => {
    const sectionHeight = section.offsetHeight;
    const sectionTop = section.offsetTop - 120;
    const sectionId = section.getAttribute('id');
    const navLink = document.querySelector(`.nav-links a[href="#${sectionId}"]`);
    if (navLink) {
      if (scrollY >= sectionTop && scrollY < sectionTop + sectionHeight) {
        navLink.classList.add('active');
      } else {
        navLink.classList.remove('active');
      }
    }
  });
};

window.addEventListener('scroll', activateNavLink);


// --- AUDIO TOGGLE ---
const audio = document.getElementById('bg-audio');
const audioBtn = document.getElementById('audio-btn');

audioBtn.addEventListener('click', () => {
  if (audio.paused) {
    audio.play();
    audioBtn.textContent = '🔈';
  } else {
    audio.pause();
    audioBtn.textContent = '🔇';
  }
});

// --- PDF MODAL ---
const pdfModal = document.getElementById('pdf-modal');
const pdfContainer = document.getElementById('pdf-container');

function openPdfModal(pdfPath, title) {
  // Try iframe first, with download fallback
  pdfContainer.innerHTML = `
    <iframe src="${pdfPath}" title="${title}"></iframe>
    <div class="pdf-fallback" style="display:none;" id="pdf-fallback">
      <p style="color: var(--text-secondary); margin-bottom: 12px;">
        Unable to display PDF in browser.
      </p>
      <a href="${pdfPath}" download>📥 Download Certificate</a>
    </div>
  `;

  // Show fallback if iframe fails
  const iframe = pdfContainer.querySelector('iframe');
  iframe.onerror = () => {
    iframe.style.display = 'none';
    document.getElementById('pdf-fallback').style.display = 'block';
  };

  pdfModal.classList.add('active');
  document.body.classList.add('modal-open');
}

function closePdfModal() {
  pdfModal.classList.remove('active');
  document.body.classList.remove('modal-open');
  // Clear iframe to stop any loading
  setTimeout(() => {
    pdfContainer.innerHTML = '';
  }, 300);
}

// --- LAB DETAIL MODAL ---
const labModal = document.getElementById('lab-modal');
let currentSlide = 0;
const totalSlides = 3;

function openLabModal() {
  labModal.classList.add('active');
  document.body.classList.add('modal-open');
  goToSlide(0);
}

function closeLabModal() {
  labModal.classList.remove('active');
  document.body.classList.remove('modal-open');
}

function updateCarousel() {
  const track = document.getElementById('carousel-track');
  track.style.transform = `translateX(-${currentSlide * 100}%)`;

  // Update dots
  const dots = document.querySelectorAll('#carousel-dots .dot');
  dots.forEach((dot, i) => {
    dot.classList.toggle('active', i === currentSlide);
  });
}

function carouselPrev() {
  currentSlide = (currentSlide - 1 + totalSlides) % totalSlides;
  updateCarousel();
}

function carouselNext() {
  currentSlide = (currentSlide + 1) % totalSlides;
  updateCarousel();
}

function goToSlide(index) {
  currentSlide = index;
  updateCarousel();
}

// --- CLOSE MODALS WITH ESCAPE KEY ---
document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') {
    if (pdfModal.classList.contains('active')) closePdfModal();
    if (labModal.classList.contains('active')) closeLabModal();
  }
});

// Close modals on overlay click (not content)
[pdfModal, labModal].forEach(modal => {
  modal.addEventListener('click', (e) => {
    if (e.target === modal) {
      if (modal === pdfModal) closePdfModal();
      if (modal === labModal) closeLabModal();
    }
  });
});

// --- EMAILJS CONTACT FORM ---
// =====================================================
// SETUP INSTRUCTIONS:
// 1. Go to https://www.emailjs.com/ and create a FREE account
// 2. Add an Email Service (Gmail) → get your SERVICE_ID
// 3. Create an Email Template with variables:
//    - {{from_name}}  → sender's name
//    - {{from_email}} → sender's email
//    - {{message}}    → the message content
//    Set the template's "To Email" to: rakhadivanda@gmail.com
// 4. Go to Account → API Keys → get your PUBLIC_KEY
// 5. Replace the placeholders below with your real IDs
// =====================================================

const EMAILJS_PUBLIC_KEY = 'YOUR_PUBLIC_KEY';       // ← Replace with your EmailJS Public Key
const EMAILJS_SERVICE_ID = 'YOUR_SERVICE_ID';       // ← Replace with your EmailJS Service ID
const EMAILJS_TEMPLATE_ID = 'YOUR_TEMPLATE_ID';     // ← Replace with your EmailJS Template ID

// Initialize EmailJS
(function () {
  if (typeof emailjs !== 'undefined') {
    emailjs.init(EMAILJS_PUBLIC_KEY);
  }
})();

const contactForm = document.getElementById('contact-form');
const submitBtn = document.getElementById('submit-btn');

contactForm.addEventListener('submit', function (e) {
  e.preventDefault();

  // Validate
  const name = document.getElementById('from_name').value.trim();
  const email = document.getElementById('from_email').value.trim();
  const message = document.getElementById('message').value.trim();

  if (!name || !email || !message) {
    showToast('Please fill in all fields.', 'error');
    return;
  }

  // Disable button
  submitBtn.disabled = true;
  submitBtn.textContent = 'Sending...';

  // Check if EmailJS is configured
  if (EMAILJS_PUBLIC_KEY === 'YOUR_PUBLIC_KEY') {
    // Demo mode — show success without actually sending
    setTimeout(() => {
      showToast('✨ Contact form is set up! Configure EmailJS to enable real email sending.', 'success');
      contactForm.reset();
      submitBtn.disabled = false;
      submitBtn.textContent = 'Send Message →';
    }, 1000);
    return;
  }

  // Send email via EmailJS
  emailjs.sendForm(EMAILJS_SERVICE_ID, EMAILJS_TEMPLATE_ID, this)
    .then(() => {
      showToast('✅ Message sent successfully! I\'ll get back to you soon.', 'success');
      contactForm.reset();
    })
    .catch((error) => {
      console.error('EmailJS Error:', error);
      showToast('❌ Failed to send message. Please try again or email me directly.', 'error');
    })
    .finally(() => {
      submitBtn.disabled = false;
      submitBtn.textContent = 'Send Message →';
    });
});

// --- TOAST NOTIFICATION ---
function showToast(message, type = 'success') {
  const toast = document.getElementById('toast');
  toast.textContent = message;
  toast.className = `toast ${type} show`;

  setTimeout(() => {
    toast.classList.remove('show');
  }, 4000);
}
