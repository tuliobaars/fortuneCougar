/* ==========================================================================
   fortuneCougar - front-end animation layer.
   The R server decides every outcome; this file only *presents* it:
   reel spins, cascades/respins, wheels, roulette, chips, HUD counters,
   toasts, celebrations and localStorage persistence.
   ========================================================================== */
(function () {
  'use strict';

  var SAVE_KEY = 'fc_state_v1';      // profile (browser storage backend / legacy)
  var ID_KEY = 'fc_player_id';       // anonymous player id (database key)
  var LANG_KEY = 'fc_lang';
  var LANG = window.FC_LANG || 'en';
  var LOCALE = LANG === 'pt_BR' ? 'pt-BR' : 'en-US';
  var I18N = window.FC_I18N || {};
  function t(s) { return I18N[s] || s; }
  var FC = window.FC = {
    turbo: false,
    busyUntil: 0,
    pendingHud: null,
    shownCoins: null,
    auto: {},
    celebrations: [],
    celebrating: false
  };
  try { FC.turbo = localStorage.getItem('fc_turbo') === '1'; } catch (e) {}

  // ---------------------------------------------------------------- utils
  function fmt(n) { return Math.round(Number(n) || 0).toLocaleString(LOCALE); }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function T(normal, turbo) { return FC.turbo ? turbo : normal; }
  function $(sel, root) { return (root || document).querySelector(sel); }
  function $$(sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); }
  function byId(id) { return document.getElementById(id); }
  function esc(s) { var d = document.createElement('div'); d.textContent = String(s == null ? '' : s); return d.innerHTML; }
  FC.fmt = fmt;

  function countUp(el, from, to, dur) {
    if (!el) return;
    from = Number(from) || 0; to = Number(to) || 0;
    if (el._raf) cancelAnimationFrame(el._raf);
    if (dur <= 0 || from === to) { el.textContent = fmt(to); return; }
    var t0 = performance.now();
    function step(t) {
      var p = Math.min(1, (t - t0) / dur);
      var e = 1 - Math.pow(1 - p, 3);
      el.textContent = fmt(from + (to - from) * e);
      if (p < 1) el._raf = requestAnimationFrame(step);
    }
    el._raf = requestAnimationFrame(step);
  }
  FC.countUp = countUp;

  function markBusy(ms) { FC.busyUntil = Math.max(FC.busyUntil, Date.now() + ms); }
  function releaseBusy() {
    FC.busyUntil = 0;
    if (FC.pendingHud) { var m = FC.pendingHud; FC.pendingHud = null; applyHud(m); }
  }

  // ------------------------------------------------------------------ HUD
  function applyHud(m, skipCoins) {
    var lv = byId('hud-level'); if (lv) lv.textContent = m.level;
    var xp = byId('hud-xp'); if (xp) xp.style.width = Math.min(100, m.xp_pct) + '%';
    var st = byId('hud-stars'); if (st) st.textContent = fmt(m.stars);
    var vip = byId('hud-vip'); if (vip) { vip.textContent = m.vip; vip.style.borderColor = m.vip_color; }
    var av = byId('hud-avatar'); if (av) av.textContent = m.avatar;
    var nm = byId('hud-name-txt'); if (nm && m.name != null) nm.textContent = m.name;
    if (!skipCoins) {
      var c = byId('hud-coins');
      var from = FC.shownCoins == null ? m.coins : FC.shownCoins;
      countUp(c, from, m.coins, Math.abs(m.coins - from) > 0 ? T(700, 250) : 0);
      if (c && m.coins > from) { c.parentNode.classList.remove('gain'); void c.offsetWidth; c.parentNode.classList.add('gain'); }
      FC.shownCoins = m.coins;
    }
  }
  function setCoinsNow(v) {
    var c = byId('hud-coins');
    countUp(c, FC.shownCoins == null ? v : FC.shownCoins, v, T(300, 120));
    FC.shownCoins = v;
  }

  // --------------------------------------------------------------- toasts
  function toast(m) {
    var box = byId('fc-toasts'); if (!box) return;
    var el = document.createElement('div');
    el.className = 'fc-toast ' + (m.kind || 'info');
    el.innerHTML = '<div class="ht-icon">' + (m.icon || '') + '</div><div class="ht-body"><div class="ht-title">' +
      esc(m.title) + '</div><div class="ht-text">' + (m.body || '') + '</div></div>';
    el.onclick = function () { el.remove(); };
    box.appendChild(el);
    while (box.children.length > 4) box.removeChild(box.firstChild);
    setTimeout(function () { el.classList.add('out'); setTimeout(function () { el.remove(); }, 400); }, 4600);
  }

  // --------------------------------------------------------- celebrations
  var CELEB = {
    win: [t('WIN!'), '\uD83E\uDE99'], big: [t('BIG WIN!'), '\uD83D\uDCB0'], mega: [t('MEGA WIN!'), '\uD83D\uDC8E'],
    epic: [t('EPIC WIN!'), '\uD83D\uDC51'], jackpot: [t('JACKPOT!'), '\uD83C\uDFB0'], level: [t('LEVEL UP!'), '\u2B50'],
    album: [t('COMPLETE!'), '\uD83C\uDFC6'], vip: [t('NEW STATUS!'), '\uD83D\uDC51']
  };
  function celebrate(m) {
    FC.celebrations.push(m);
    if (!FC.celebrating) nextCelebration();
  }
  function nextCelebration() {
    var m = FC.celebrations.shift();
    var box = byId('fc-celebrate');
    if (!m || !box) { FC.celebrating = false; return; }
    FC.celebrating = true;
    var c = CELEB[m.kind] || CELEB.win;
    var coins = '';
    for (var i = 0; i < 26; i++) {
      coins += '<span class="coin-drop" style="left:' + (Math.random() * 100).toFixed(1) + '%;animation-delay:' +
        (Math.random() * 0.9).toFixed(2) + 's;animation-duration:' + (1.4 + Math.random()).toFixed(2) + 's">' +
        (m.kind === 'level' || m.kind === 'vip' ? '\u2B50' : '\uD83E\uDE99') + '</span>';
    }
    box.innerHTML = '<div class="celebrate-inner ' + esc(m.kind) + '"><div class="cb-burst"></div>' + coins +
      '<div class="cb-icon">' + c[1] + '</div><div class="cb-title">' + esc(m.title || c[0]) + '</div>' +
      (m.amount > 0 ? '<div class="cb-amount">0</div>' : '') +
      (m.subtitle ? '<div class="cb-sub">' + esc(m.subtitle) + '</div>' : '') +
      '<div class="cb-tap">' + esc(t('tap to continue')) + '</div></div>';
    box.className = 'show';
    countUp($('.cb-amount', box), 0, m.amount, T(1400, 500));
    var done = false;
    function close() {
      if (done) return; done = true;
      box.className = '';
      setTimeout(nextCelebration, 250);
    }
    box.onclick = close;
    setTimeout(close, T(2800, 1300));
  }

  // ------------------------------------------------------------- slot reels
  function symbolMap(machine) {
    if (!machine._symbols) machine._symbols = JSON.parse(machine.getAttribute('data-symbols') || '{}');
    return machine._symbols;
  }
  function setCell(cell, map, id, badge, extra) {
    var s = map[id] || ['', ''];
    cell.className = 'cell ' + (s[1] || '') + (extra ? ' ' + extra : '');
    cell.setAttribute('data-sym', id);
    cell.innerHTML = '<div class="sym">' + s[0] + '</div><span class="cell-badge">' + esc(badge || '') + '</span>';
  }
  function reelCells(machine) {
    return $$('.reel', machine).map(function (r) { return $$('.cell', r); });
  }
  function randomId(map) {
    var keys = Object.keys(map).filter(function (k) { return k !== 'blank'; });
    return keys[Math.floor(Math.random() * keys.length)];
  }
  function showNote(machine, text) {
    var n = $('.reel-note', machine); if (!n) return;
    if (!text) { n.classList.remove('show'); return; }
    n.textContent = text; n.classList.remove('show'); void n.offsetWidth; n.classList.add('show');
  }
  function banner(machine, html, cls, ms) {
    var b = $('.win-banner', machine); if (!b) return;
    b.innerHTML = html; b.className = 'win-banner show ' + (cls || '');
    clearTimeout(b._t); b._t = setTimeout(function () { b.className = 'win-banner'; }, ms || 1600);
  }

  function applyFrame(machine, f, opts) {
    opts = opts || {};
    var map = symbolMap(machine), cols = reelCells(machine);
    var anyWin = false;
    for (var c = 0; c < cols.length; c++) for (var r = 0; r < cols[c].length; r++) if (f.h[r][c]) anyWin = true;
    for (c = 0; c < cols.length; c++) {
      if (opts.onlyReel != null && opts.onlyReel !== c) continue;
      for (r = 0; r < cols[c].length; r++) {
        var extra = [f.c[r][c] || ''];
        if (f.k && f.k[r][c]) extra.push('held');
        if (opts.hl !== false && anyWin) extra.push(f.h[r][c] ? 'win' : 'dim');
        if (opts.anim) extra.push(opts.anim);
        setCell(cols[c][r], map, f.g[r][c], f.b[r][c], extra.join(' '));
      }
    }
  }

  // A spinning reel shows a pre-rendered strip of random symbols that is only
  // *translated* by CSS (compositor-only), so spins stay smooth on any device.
  function makeStrip(map, n) {
    var strip = document.createElement('div');
    strip.className = 'reel-strip';
    var html = '';
    for (var i = 0; i < n * 2; i++) {
      var s = map[randomId(map)] || ['', ''];
      html += '<div class="strip-cell ' + (s[1] || '') + '"><div class="sym">' + s[0] + '</div></div>';
    }
    strip.innerHTML = html;
    return strip;
  }

  async function spinReels(machine, f) {
    var map = symbolMap(machine), cols = reelCells(machine), n = cols.length;
    var stagger = T(170, 55), base = T(420, 140);
    var reels = $$('.reel', machine);
    reels.forEach(function (reel, c) {
      var held = cols[c].map(function (cell, r) { return !!(f.k && f.k[r][c]); });
      if (held.some(Boolean)) {
        // sticky / held symbols stay visible: only the free cells shimmer
        cols[c].forEach(function (cell, r) { if (!held[r]) cell.classList.add('shimmer'); });
        reel.classList.add('has-held');
      } else {
        reel.appendChild(makeStrip(map, cols[c].length + 2));
        reel.classList.add('spinning');
      }
    });
    for (var c = 0; c < n; c++) {
      await sleep(c === 0 ? base : stagger);
      var strip = $('.reel-strip', reels[c]); if (strip) strip.remove();
      reels[c].classList.remove('spinning', 'has-held');
      applyFrame(machine, f, { onlyReel: c, hl: false, anim: 'land' });
    }
    await sleep(T(140, 40));
  }

  async function respinCells(machine, f) {
    var map = symbolMap(machine), cols = reelCells(machine);
    cols.forEach(function (col, c) {
      col.forEach(function (cell, r) {
        if (!(f.k && f.k[r][c])) { setCell(cell, map, randomId(map), '', 'shimmer'); }
      });
    });
    await sleep(T(420, 140));
  }

  async function playSpin(m) {
    var machine = byId(m.machine); if (!machine) return;
    var spinBtn = byId(machine.getAttribute('data-spin'));
    var total = 0;
    markBusy(60000);
    if (spinBtn) spinBtn.classList.add('busy');
    machine.classList.add('is-spinning');
    var winEl = $('.win-val', machine);
    if (!m.free && winEl) winEl.textContent = '0';
    // show the stake leaving the balance immediately
    if (!m.free && FC.shownCoins != null) setCoinsNow(m.balance - m.win);
    showNote(machine, '');
    try {
      for (var i = 0; i < m.frames.length; i++) {
        var f = m.frames[i];
        if (i === 0 || f.m === 'spin') {
          await spinReels(machine, f);
        } else if (f.m === 'cascade') {
          $$('.cell.win', machine).forEach(function (c) { c.classList.add('explode'); });
          await sleep(T(380, 120));
          applyFrame(machine, f, { hl: false });
          await sleep(T(360, 110));
        } else if (f.m === 'respin') {
          await respinCells(machine, f);
          applyFrame(machine, f, { hl: false, anim: 'land' });
          await sleep(T(200, 60));
        } else { // swap: expand / transform / nudge / reveal
          await sleep(T(380, 120));
          applyFrame(machine, f, { hl: false });
          await sleep(T(380, 120));
        }
        if (f.n) showNote(machine, f.n);
        applyFrame(machine, f, { hl: true });
        if (f.w > 0 && winEl) countUp(winEl, total, f.w, T(450, 150));
        if (f.w > 0) total = f.w;
        if (i < m.frames.length - 1) await sleep(T(520, 160));
      }
      if (winEl) countUp(winEl, total, m.win, T(500, 150));
      if (m.fs_awarded > 0) banner(machine, '<b>' + m.fs_awarded + '</b> ' + esc(t('FREE SPINS!')), 'fs', 2000);
      else if (m.fs_ended) banner(machine, esc(t('FREE SPINS WON')) + ' <b>' + fmt(m.fs_won) + '</b>', 'fs', 2200);
      else if (m.win > 0 && (m.tier === 'win')) banner(machine, esc(t('WIN')) + ' <b>' + fmt(m.win) + '</b>', 'small', 1300);
      if (m.tier === 'big' || m.tier === 'mega' || m.tier === 'epic') {
        celebrate({ kind: m.tier, amount: m.win, title: null, subtitle: $('h3', machine) ? $('h3', machine).textContent : '' });
      }
      if (m.bonus) banner(machine, '\uD83C\uDF81 ' + esc(t('BONUS!')), 'fs', 1800);
    } finally {
      machine.classList.remove('is-spinning');
      if (spinBtn) spinBtn.classList.remove('busy');
      updateSpinLabel(machine, m);
      releaseBusy();
      var autoBtn = $('.btn-auto', machine);
      if (autoBtn && FC.auto[autoBtn.id] > 0) {
        if (m.stop_auto) stopAuto(autoBtn.id);
        else {
          FC.auto[autoBtn.id]--;
          autoBtn.querySelector('.auto-n') && (autoBtn.querySelector('.auto-n').textContent = FC.auto[autoBtn.id]);
          if (FC.auto[autoBtn.id] <= 0) stopAuto(autoBtn.id);
          else setTimeout(function () { if (FC.auto[autoBtn.id] > 0 && spinBtn) spinBtn.click(); }, T(350, 90));
        }
      }
    }
  }
  FC.queue = Promise.resolve();

  function updateSpinLabel(machine, m) {
    var spinBtn = byId(machine.getAttribute('data-spin')); if (!spinBtn) return;
    var lab = spinBtn.querySelector('.spin-label'); if (!lab) return;
    if (m.fs_left > 0) { lab.innerHTML = esc(t('FREE')) + '<small>' + m.fs_left + ' ' + esc(t('left')) + '</small>'; spinBtn.classList.add('free'); }
    else { lab.textContent = t('SPIN'); spinBtn.classList.remove('free'); }
  }

  // Auto-spin
  function stopAuto(id) {
    FC.auto[id] = 0;
    var b = byId(id); if (b) { b.classList.remove('on'); b.textContent = t('AUTO'); }
  }
  function toggleAuto(btn) {
    if (FC.auto[btn.id] > 0) { stopAuto(btn.id); return; }
    FC.auto[btn.id] = 50;
    btn.classList.add('on');
    btn.innerHTML = esc(t('STOP')) + ' <span class="auto-n">50</span>';
    var spin = byId(btn.getAttribute('data-spin'));
    if (spin && !spin.classList.contains('busy')) spin.click();
  }

  // ------------------------------------------------------------ prize wheels
  function spinPrizeWheel(w) {
    if (w._spun) return; w._spun = true;
    var k = parseInt(w.getAttribute('data-spin-to'), 10), n = parseInt(w.getAttribute('data-n'), 10);
    var dur = parseInt(w.getAttribute('data-duration'), 10) || 4200;
    dur = T(dur, Math.round(dur * 0.45));
    var rotor = $('.wheel-rotor', w); if (!rotor || !n) return;
    var seg = 360 / n;
    var target = 360 * 6 - (k - 1) * seg + (Math.random() - 0.5) * seg * 0.6;
    rotor.style.transition = 'none'; rotor.style.transform = 'rotate(0deg)';
    markBusy(dur + 200);
    requestAnimationFrame(function () { requestAnimationFrame(function () {
      rotor.style.transition = 'transform ' + dur + 'ms cubic-bezier(.15,.7,.12,1)';
      rotor.style.transform = 'rotate(' + target + 'deg)';
    }); });
    setTimeout(function () { w.classList.remove('spinning'); w.classList.add('landed'); releaseBusy(); }, dur + 80);
  }
  function scanWheels(root) {
    $$('.prize-wheel[data-spin-to]', root).forEach(spinPrizeWheel);
  }

  // ------------------------------------------------------------------ roulette
  function spinRoulette(m) {
    var dur = T(m.duration, Math.round(m.duration * 0.4));
    markBusy(dur + 300);
    m.wheels.forEach(function (w, i) {
      var el = byId(w.id); if (!el) return;
      var rotor = $('.rl-rotor', el), track = $('.rl-ball-track', el), bubble = $('.rl-result-bubble', el);
      var seg = 360 / w.n;
      var cur = parseFloat(el.getAttribute('data-rot') || '0');
      var target = -w.index * seg;
      var delta = ((target - cur) % 360 + 360) % 360;
      var next = cur + 360 * (4 + i % 2) + delta;
      el.setAttribute('data-rot', next);
      if (bubble) bubble.className = 'rl-result-bubble';
      rotor.style.transition = 'transform ' + dur + 'ms cubic-bezier(.12,.62,.1,1)';
      rotor.style.transform = 'rotate(' + next + 'deg)';
      track.style.transition = 'none'; track.style.transform = 'rotate(0deg)';
      el.classList.add('spinning');
      requestAnimationFrame(function () { requestAnimationFrame(function () {
        track.style.transition = 'transform ' + dur + 'ms cubic-bezier(.2,.6,.15,1)';
        track.style.transform = 'rotate(' + (-360 * (6 + i % 3)) + 'deg)';
      }); });
      setTimeout(function () {
        el.classList.remove('spinning');
        if (bubble) { bubble.textContent = w.label; bubble.className = 'rl-result-bubble show ' + w.color; }
      }, dur + 50);
    });
    setTimeout(function () {
      var last = byId(m.last); if (last) last.innerHTML = m.result_html;
      var hist = byId(m.history); if (hist) hist.innerHTML = m.history_html;
      releaseBusy();
    }, dur + 120);
  }
  function paintChips(m) {
    var table = byId(m.table); if (!table) return;
    $$('.rl-chip', table).forEach(function (c) { c.remove(); });
    Object.keys(m.bets || {}).forEach(function (k) {
      var spot = table.querySelector('[data-key="' + k + '"]');
      if (!spot) return;
      var v = m.bets[k];
      var chip = document.createElement('span');
      chip.className = 'rl-chip ' + chipClass(v);
      chip.textContent = short(v);
      spot.appendChild(chip);
    });
  }
  function chipClass(v) {
    return v >= 100000 ? 'chip-black' : v >= 25000 ? 'chip-purple' : v >= 5000 ? 'chip-orange' :
      v >= 1000 ? 'chip-blue' : v >= 500 ? 'chip-green' : 'chip-red';
  }
  function short(v) {
    var dm = LANG === 'pt_BR' ? ',' : '.';
    if (v >= 1e6) return String(Math.round(v / 1e5) / 10).replace('.', dm) + 'M';
    if (v >= 1e3) return String(Math.round(v / 100) / 10).replace('.', dm) + 'K';
    return String(v);
  }
  FC.selectChip = function (el) {
    $$('.chip', el.parentNode).forEach(function (c) { c.classList.remove('selected'); });
    el.classList.add('selected');
  };

  // ------------------------------------------------------------------ turbo
  FC.toggleTurbo = function () {
    FC.turbo = !FC.turbo;
    try { localStorage.setItem('fc_turbo', FC.turbo ? '1' : '0'); } catch (e) {}
    syncTurbo();
    toast({ title: t(FC.turbo ? 'Turbo ON' : 'Turbo OFF'), body: t(FC.turbo ? 'Animations at hyper speed.' : 'Full animations.'), kind: 'info', icon: '\u26A1' });
  };
  function syncTurbo() {
    document.body.classList.toggle('turbo', FC.turbo);
    var b = byId('hud-turbo'); if (b) b.classList.toggle('on', FC.turbo);
  }

  // ------------------------------------------------------------ Shiny hooks
  function register() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return false;
    Shiny.addCustomMessageHandler('fc_hud', function (m) {
      if (Date.now() < FC.busyUntil) { FC.pendingHud = m; applyHud(m, true); }
      else applyHud(m);
    });
    Shiny.addCustomMessageHandler('fc_toast', function (m) {
      var wait = Math.max(0, FC.busyUntil - Date.now());
      setTimeout(function () { toast(m); }, Math.min(wait, 2500));
    });
    Shiny.addCustomMessageHandler('fc_celebrate', function (m) { setTimeout(function () { celebrate(m); }, T(m.delay || 0, (m.delay || 0) * 0.45)); });
    Shiny.addCustomMessageHandler('fc_save', function (m) { try { localStorage.setItem(SAVE_KEY, m.data); } catch (e) {} });
    Shiny.addCustomMessageHandler('fc_set_id', function (m) { try { localStorage.setItem(ID_KEY, m.id); } catch (e) {} });
    Shiny.addCustomMessageHandler('fc_spin', function (m) { FC.queue = FC.queue.then(function () { return playSpin(m); }); });
    Shiny.addCustomMessageHandler('fc_auto_stop', function (m) { stopAuto(m.id); });
    Shiny.addCustomMessageHandler('fc_win_value', function (m) {
      setTimeout(function () { var el = byId(m.id); if (el) countUp(el, 0, m.value, 600); }, T(m.delay || 0, (m.delay || 0) * 0.45));
    });
    Shiny.addCustomMessageHandler('fc_roulette', spinRoulette);
    Shiny.addCustomMessageHandler('fc_chips', paintChips);
    return true;
  }
  if (!register()) document.addEventListener('DOMContentLoaded', register);

  // ---- player id & language -----------------------------------------------
  function newId() {
    if (window.crypto && crypto.randomUUID) return crypto.randomUUID();
    var s = ''; for (var i = 0; i < 32; i++) s += Math.floor(Math.random() * 16).toString(16);
    return s;
  }
  function playerId() {
    var id = null;
    try { id = localStorage.getItem(ID_KEY); if (!id) { id = newId(); localStorage.setItem(ID_KEY, id); } } catch (e) { id = newId(); }
    return id;
  }
  FC.playerId = playerId;
  // Switch language: remember the choice and reload with ?lang=
  FC.setLang = function (lang) {
    try { localStorage.setItem(LANG_KEY, lang); } catch (e) {}
    var u = new URL(window.location.href);
    u.searchParams.set('lang', lang);
    window.location.href = u.toString();
  };
  // A remembered language wins over the app default when the URL has none.
  (function () {
    try {
      var pref = localStorage.getItem(LANG_KEY);
      var u = new URL(window.location.href);
      if (pref && !u.searchParams.get('lang') && pref !== LANG) { u.searchParams.set('lang', pref); window.location.replace(u.toString()); }
    } catch (e) {}
  })();

  // Shiny fires 'shiny:connected' through jQuery, so listen with jQuery.
  function onConnected() {
    if (FC._helloSent) return; FC._helloSent = true;
    var saved = '';
    try { saved = localStorage.getItem(SAVE_KEY) || ''; } catch (e) {}
    Shiny.setInputValue('fc_hello', { id: playerId(), saved: saved }, { priority: 'event' });
  }
  function hookConnected() {
    if (window.jQuery) { jQuery(document).on('shiny:connected', onConnected); return true; }
    return false;
  }
  if (!hookConnected()) document.addEventListener('DOMContentLoaded', hookConnected);

  // ---- player name dialog: focus the field; Enter submits --------------------
  // (the typed value is pushed first, so a fast Enter never submits a stale name)
  function pushName() {
    var i = document.querySelector('.fc-name-modal input');
    if (i && window.Shiny && Shiny.setInputValue) Shiny.setInputValue(i.id, i.value);
  }
  function hookNameModal() {
    if (!window.jQuery) return false;
    jQuery(document).on('shown.bs.modal', function () {
      var i = document.querySelector('.fc-name-modal input');
      if (i) { i.focus(); i.select(); }
    });
    jQuery(document).on('keydown', '.fc-name-modal input', function (e) {
      if (e.key === 'Enter') {
        e.preventDefault(); pushName();
        var b = document.querySelector('.fc-name-ok'); if (b) setTimeout(function () { b.click(); }, 0);
      }
    });
    jQuery(document).on('mousedown touchstart', '.fc-name-ok', pushName);
    return true;
  }
  if (!hookNameModal()) document.addEventListener('DOMContentLoaded', hookNameModal);

  document.addEventListener('DOMContentLoaded', function () {
    syncTurbo();
    scanWheels(document);
    new MutationObserver(function (muts) {
      muts.forEach(function (mu) {
        mu.addedNodes.forEach(function (n) { if (n.nodeType === 1) scanWheels(n.parentNode || n); });
      });
    }).observe(document.body, { childList: true, subtree: true });
  });

  document.addEventListener('click', function (e) {
    var a = e.target.closest && e.target.closest('.btn-auto');
    if (a) { e.preventDefault(); toggleAuto(a); }
  });

  // Space = spin the visible machine, T = turbo
  document.addEventListener('keydown', function (e) {
    var tag = (e.target && e.target.tagName || '').toLowerCase();
    if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
    if (e.code === 'Space') {
      var m = $$('.slot-machine').filter(function (x) { return x.offsetParent !== null; })[0];
      if (!m) return;
      e.preventDefault();
      var b = byId(m.getAttribute('data-spin'));
      // a focused button would also fire a native click on keyup -> blur it
      if (document.activeElement && document.activeElement.tagName === 'BUTTON') document.activeElement.blur();
      if (b && !b.classList.contains('busy')) b.click();
    } else if (e.key === 't' || e.key === 'T') {
      FC.toggleTurbo();
    }
  });
})();
