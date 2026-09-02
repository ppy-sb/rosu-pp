//! Expected judgement counts as a function of player skill.
//!
//! [`crate::mania::sunny_windows`] says how wide each judgement window is; this module
//! says how likely a player is to land inside one. Together they form the
//! "accuracy surface": for a fixed map and mod combination, a family of six
//! curves giving the expected count of each judgement at every skill level.
//!
//! # Why this exists
//!
//! The current pipeline multiplies pp by an accuracy multiplier that sees only
//! the score's accuracy and one map-wide scalar. It cannot tell that 99% on a
//! map with a 22.5ms PERFECT window is easier than 99% on one with a 16.5ms
//! window, so mods that widen windows have to be corrected for afterwards with
//! a flat multiplier. Reading skill off this surface removes the need for that
//! correction: a wider window shifts the whole curve family, so identical
//! judgement counts simply imply less skill.
//!
//! # Model
//!
//! Each judgement unit — a note, or an LN head and tail separately when they are
//! judged that way — has a local difficulty `d` in star-rating units. A player of
//! skill `s` hitting it produces a timing error drawn from a zero-mean normal
//! distribution whose spread grows with how far they are over-reaching:
//!
//! ```text
//! sigma(d, s) = SIGMA_REF * ((d + D_FLOOR) / s) ^ SKILL_EXPONENT
//! ```
//!
//! `D_FLOOR` keeps trivial patterns from being free, so a 1-star map still does
//! not yield an all-PERFECT score at ordinary skill, while still letting sigma
//! reach zero as skill grows without bound — a genuine 100% stays attainable
//! instead of saturating the surface.
//!
//! Each judgement's probability is the mass the error distribution places inside
//! its band. Because the bands partition the real line, the six expected counts
//! always sum to the number of judgement units: these are one distribution seen
//! six ways, not six independent fits.
//!
//! # What the skill scale means
//!
//! Skill is in the same units as sunny's difficulty values, because the model only
//! ever uses the two as a ratio. This is a deliberate choice and the reason the
//! surface has no difficulty model of its own: sunny decides what is hard, the
//! surface decides what a given accuracy implies about the player, and a single
//! scale means the same thing on a 2-star map and a 20-star one.
//!
//! The scale has to cover the entire userbase, from someone who cannot play the map
//! at all to someone who SSes it comfortably, and it does — but the informative
//! region is a band around the map's difficulty rather than a fixed range of
//! numbers. Measured on OD9 with even difficulty, accuracy crosses 1% at ~0.04x
//! difficulty, 50% at ~0.4x, 99% at ~1.2x, and saturates at 1.0 around
//! [`SKILL_SATURATION_RATIO`]. That span is ~110x in skill at every difficulty
//! tested, so the same ratios apply throughout: a d=20 map needs roughly ten times
//! the skill of a d=2 map for the same accuracy.
//!
//! Both ends of that range are floors rather than measurements, and
//! [`FitQuality::is_identifiable`] reports the lower one. Above the saturation ratio
//! every note is a near-certain 320 and the likelihood flattens, so a fitted skill
//! on an SS is a lower bound — irrelevant for pp, where every such score should
//! score alike, but it means SS scores cannot pin a value during calibration. Below
//! [`SKILL_IDENTIFIABLE_MIN`] the fit stops resolving at all, for reasons specific
//! to how misses are handled; see below.
//!
//! # Robustness to real scores
//!
//! Real plays do not follow the model. A player's skill drifts within a single
//! run, their timing is biased by audio latency, they choke one section, and they
//! drop notes for reasons that have nothing to do with timing precision. The
//! numbers below come from simulation over 200-300 synthetic scores each and are
//! pinned by tests.
//!
//! The reassuring part is the direction of the error. Every form of heterogeneity
//! tested biases the skill estimate *downward*, because a mixture of easy and hard
//! notes always produces a wider judgement spread than any single skill level can,
//! and the fit answers with the lower skill that explains the spread:
//!
//! | Deviation | Effect on estimated skill |
//! |---|---|
//! | Skill varies ±20% within the play | −7% |
//! | Skill varies ±50% within the play | −38% |
//! | Per-note difficulty varies ±40% around the assumed value | −10% |
//! | 20% of notes hit with 3× the error spread | −19% |
//! | Constant 12ms timing offset | −8% |
//!
//! Only one thing biases upward: a player *more* consistent than a normal
//! distribution predicts (+15% when 40% of notes are hit unusually tightly). That
//! is a real limitation, but it is not an exploit — it cannot be induced by mod
//! choice or map choice, only by playing better than the model expects.
//!
//! Two mechanisms handle the rest.
//!
//! # Misses are conditioned out of the fit
//!
//! [`log_likelihood`] scores only the five timing bands, renormalised among
//! themselves, and ignores the miss count entirely. The reason is that misses are
//! informative about timing only for players bad enough to have them *from* timing.
//! On an OD9 map the MEH window ends at 124.5 ms, so a miss requires an error past
//! that. The timing model reaches it readily at low skill — at difficulty 5 it
//! predicts 600 misses per 1300 notes at skill 1.5, and 22 at skill 3 — and then
//! stops: from skill 4 up it predicts none at all, at any map length.
//!
//! So for any competent score, every miss comes from outside the model: a lag spike,
//! a misread, a hand off the keys. Scoring those against a timing distribution is
//! what used to drag the estimate down, and patching it with a lapse rate made the
//! estimate depend on an uncalibrated constant. Conditioning removes both problems
//! at once, and it is strictly more robust — a score that drops 400 of 1300 notes
//! loses 66% of its estimated skill under a fitted miss channel and 9.5% under
//! conditioning, while clean scores recover their generating skill just as accurately
//! either way. It is also self-consistent: the model no longer insists on
//! misses for a score that has none, which is what almost every SS actually looks
//! like.
//!
//! None of this loses the misses. They are fully accounted for on the scoring side,
//! where they belong — they lower accuracy directly and break combo. What they no
//! longer do is corrupt the inference about how precisely the player hit the notes
//! they *did* hit. That division of labour is the point: misses are a difficulty
//! matter, handled by SR and by accuracy; the surface's job is the timing spread.
//!
//! [`ErrorModel::slip_rate`] therefore only affects [`expected_counts`] — forward
//! prediction for a population, not inference about one score — and it is **zero by
//! default**, so predicted misses come only from the timing distribution crossing
//! the MISS boundary. Two consequences, both wanted. An SS is the ordinary
//! prediction for a player comfortably above the map rather than an exponentially
//! unlikely one, and predicted misses respond to the miss window: widening it under
//! EZ moves probability mass back into MEH, so EZ predicts *fewer* misses. Under a
//! flat additive rate neither held — the rate is window-independent by construction,
//! and it made an SS on 6358 notes a `1.4e-14` event.
//!
//! ## What conditioning costs
//!
//! It gives up all resolution at the bottom of the scale. As sigma grows past the
//! miss boundary the five surviving band *shares* converge on fixed ratios set by
//! the window widths alone — 0.193 / 0.241 / 0.265 / 0.169 / 0.132 on OD9,
//! essentially converged by skill 0.3 — because an error spread of thousands of ms
//! is effectively uniform across a 161 ms line. Once there, the only thing
//! separating a player who hits 5% of notes from one who hits 40% *is* the miss
//! count, and the conditional fit has discarded it. Both come back at
//! [`SKILL_MIN`]. The full six-band likelihood does order them correctly (0.39 up to
//! 2.14 over that range).
//!
//! The trade is taken because the two regimes do not overlap. Sunny awards no pp at
//! or below 80% accuracy, and the divergence between the two fits only begins around
//! 69% — every score where they disagree already earns zero. Above that line the two
//! are identical to the digit. So conditioning buys correct handling of lag spikes in
//! the range that pays, at the price of a flat floor in the range that does not.
//! [`FitQuality::is_identifiable`] marks when a returned value is that floor, which
//! matters for calibration even though it does not for scoring.
//!
//! [`fit_with_quality`] reports *whether the fit is any good*. Some scores simply
//! are not describable by one skill level — a play that free-3 20s a vibro section
//! and drops everything else fits nothing, and comes back with a G statistic in the
//! thousands. The estimate alone would silently return a number for such a score;
//! the statistic makes it visible, which is what lets the deployment side decide
//! whether to trust it, clamp it, or flag it.
//!
//! # Two ways a real score departs from the surface
//!
//! Both occur constantly in submitted scores, and they need opposite treatment.
//!
//! A **constant offset** — audio desync, input latency, a player who hits early by
//! habit — leaves precision intact but moves the centre of the error distribution
//! off zero. The surface assumes zero, so it reads the shifted mass as imprecision.
//! On a 1300-note map at skill 6, a 10 ms offset costs 9% of the estimate, 20 ms
//! costs 25%, 45 ms costs 48%. This is the model's main blind spot: nothing in a
//! counts-only judgement vector distinguishes "offset by 20 ms" from "less precise",
//! because both move 320s into 300s and 200s. Detecting it would need the signed
//! mean error, which is replay data the design deliberately does without. What the
//! surface *can* do is notice that the resulting shape is not one it produces, and
//! `g_timing` past ~30 flags exactly that. Note the direction: the bias is always
//! downward, so an offset player is under-rated rather than over-rated.
//!
//! A **lag spike or dropped input** is the opposite: it converts hits into misses
//! and leaves the timing bands alone. The fit ignores it by construction, and it
//! should, because the timing evidence is still perfectly good — 200 dropped notes
//! out of 1300 move the skill estimate by 4%, and that residue is only the shift in
//! the surviving judgement mix. The score is worse and its accuracy already says so;
//! what has not happened is any loss of confidence in the *skill* the surviving notes
//! demonstrate.
//!
//! Telling the two apart is why the goodness of fit is reported twice. The miss
//! channel's expected count is small — `n * slip_rate`, about 6.5 notes on a
//! 1300-note map — so as soon as a score exceeds it the miss term dominates a
//! combined statistic: 50 dropped notes contribute 145 on their own, dwarfing the
//! ~0.3 coming from the timing bands. A single threshold on the combined figure
//! therefore rejects lag-spiked scores whose skill estimate is entirely sound, while
//! being no more sensitive to the offset case that actually breaks the fit.
//! [`FitQuality::is_plausible`] keys off `g_timing` for that reason, and
//! `excess_misses` reports the dropped notes separately, in counts.
//!
//! # Calibration
//!
//! [`ErrorModel`] is where every free parameter lives. [`log_likelihood`] is public
//! so it can be fitted; the harness is `sunny::tests::calibration_search`, run
//! against 20 real scores from a live server.
//!
//! **The error distribution is a two-component normal mixture, not a single normal.**
//! That was forced by the data rather than chosen. Fitting a lone normal to real
//! scores leaves a residual with a consistent signature: the observed PERFECT share
//! runs *above* prediction while the observed OK and MEH shares also run above it — a
//! sharper core and a fatter tail simultaneously. One normal has a single width and
//! must trade one against the other, so it splits the difference and misses at both
//! ends, worst on the cleanest scores. On the most extreme fixture it underpredicted
//! the OK share by 19x and MEH by 90x, for a `g_timing` of 688. Adding the lapse
//! component ([`ErrorModel::lapse_weight`], [`ErrorModel::lapse_ratio`]) cut mean
//! `g_timing` across the set from 101.6 to 51.6, and that same score from 688 to 100.
//!
//! **`sigma_ref` is a gauge parameter and cannot be calibrated.** It sets the unit
//! skill is expressed in, and skill is refit for every score, so a change in
//! `sigma_ref` is absorbed exactly by the fitted skill and no observable moves at all:
//! sweeping it over a 16x range leaves `g_timing` identical to four decimals while
//! skill scales as `sigma_ref^(1/skill_exponent)`. It is fixed at 18.0 as a
//! convention, chosen so fitted skill lands roughly on the star-rating scale.
//! `sigma_ref_only_sets_the_scale_of_skill` pins this. The corollary matters for
//! pricing: since the window scalar is a *ratio* of two fitted skills, it is invariant
//! to the choice.
//!
//! **`skill_exponent` and `difficulty_floor` are held, not fitted.** `skill_exponent`
//! is identified only by variation in the skill-to-difficulty ratio, and the fixture
//! set is one player sitting at 0.96-1.72x the star rating on every map — at a ratio
//! of 1, `sigma` equals `sigma_ref` whatever the exponent is, so the two are close to
//! jointly unidentified on this data. Since `skill_exponent` alone determines how the
//! scalar answers a window change, fitting it here would reset the entire mod response
//! on the strength of one player's idiosyncrasy. It needs a spread of skill levels.
//!
//! A wider set was since measured directly — 270 replays, 9 players, two disjoint
//! skill bands, 2.3 to 10.0 stars — and it still does not pin the exponent. Profiling
//! it against measured timing spread is nearly flat between 1.0 and 1.7 (RMS 2.30 vs
//! 2.31 ms) once [`ErrorModel::sigma_floor`] is in place, and it is the floor rather
//! than the exponent that the low-difficulty data actually constrains. Fitting the two
//! together without a floor returns an exponent near 0.8, which is floor absorption:
//! the flat easy end gets read as a weak power law. So the exponent stays at 1.7 and
//! the honest statement is that this data bounds it loosely, not that it confirms it.
//! Pinning it needs scores well past 10 stars, where the predicted spreads separate.
//!
//! **Replays are a cross-check, not the primary instrument.** Each one measures a
//! single player-session, sampled at the client's frame rate — a 60 fps replay
//! quantises every hit to ~17 ms bins, injecting `gap/sqrt(12)` ≈ 4.9 ms of noise
//! that has to be removed in quadrature before a spread means anything. They are the
//! right tool for testing what the counts-based fit infers, which is how both the
//! floor and the sigma-dependent tail below were found. Judgement counts across many
//! scores remain what the model is calibrated on.
//!
//! So the calibrated quantities are the two shape parameters, and what they are fitted
//! to is the *shape* of the residual — the absolute quality of the fit, which is why
//! [`FitQuality::is_plausible`] is the right objective here even though it is the wrong
//! gate for pricing.

// The fitting path is wired into `sunny::window_scalar`. Several forward-prediction
// helpers are not yet consumed by it — they are used by the tests and by the
// calibration work still to come, so the module keeps a narrow allowance rather than
// deleting API the fit will need.
#![allow(dead_code)]

use crate::mania::sunny_windows::{ManiaHitWindows, ManiaJudgement};

/// How timing error spreads as a player is pushed past their skill level.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ErrorModel {
    /// The timing error standard deviation, in ms, of a player whose skill
    /// exactly matches the local difficulty.
    pub sigma_ref: f64,
    /// How sharply error grows once difficulty exceeds skill. Higher values make
    /// the judgement curves knee more steeply.
    pub skill_exponent: f64,
    /// Added to local difficulty so that easy patterns are not perfectly free.
    pub difficulty_floor: f64,
    /// The irreducible timing error, in ms, that remains no matter how far a
    /// player's skill exceeds the difficulty. Added in quadrature to the
    /// skill-driven term, so sigma is `hypot(sigma_floor, skill_term)`.
    ///
    /// **Zero by default for two independent reasons.** A large floor (the ~10 ms the
    /// replays measure) is refuted outright by judgement counts, which cap it near
    /// 2 ms. A small floor in the physically defensible 1-5 ms band survives that
    /// test but is *unidentifiable* on the counts — the fit absorbs it into skill
    /// exactly — while still moving the window scalar, and so pp. Both arguments are
    /// laid out below.
    ///
    /// The replay evidence is not weak. Across 270 scores and 9 players spanning the
    /// top 6-20% and the 75-88th percentile of the ladder, on maps from 2.3 to 10.0
    /// sunny stars, measured timing spread flattens out at the easy end exactly as a
    /// floor predicts: holding [`Self::skill_exponent`] at 1.7, introducing a floor
    /// cuts residual spread from 3.45 to 2.26 ms RMS, per-player fits land in a tight
    /// 9.5-14.5 ms band while fitted skill ranges over 5.0-13.5, and quadrature fits
    /// as well as a hard `max` (2.25 vs 2.22 ms) while staying differentiable.
    ///
    /// It is nonetheless refuted by counts. The PERFECT window is 16.5 ms at OD 8, so
    /// a floor caps the achievable 320 share: at 10 ms no score can exceed 87%, and
    /// even 1.5 ms is set by a real 1506-note score judged entirely 320. Inverting
    /// the cleanest live no-mod scores through the mixture gives core sigma bounds of
    /// 1.5-3.3 ms, an order of magnitude under the replay figure.
    ///
    /// The two datasets do not overlap, which is how both can be internally
    /// consistent: the replay ladder was selected at `acc between 88 and 99.5`, so it
    /// never sampled a saturating score, and a floor only binds where scores
    /// saturate. Something inflates measured replay spread at the low end — frame
    /// quantisation contributes ~4.9 ms at 60 fps but not the rest, and drifting
    /// audio offset within a map is the untested candidate, since a whole-map sd
    /// absorbs drift that no per-note model should. Until that is settled this stays
    /// zero rather than pricing scores on the disputed number.
    ///
    /// **A physically motivated 1-5 ms was tested separately and also stays off, for
    /// a different reason.** The argument for it does not depend on the replays at
    /// all: osu! judges at 1000 ticks per second, so 1 ms is a hard limit on the
    /// timing anyone can resolve, and keyboard scan plus OS scheduling jitter add a
    /// few ms above that. Both objections to 10 ms were checked against this band and
    /// neither disposes of it — the counts allow up to about 2 ms
    /// (`the_counts_bound_a_floor_near_two_milliseconds`), and de-quantising the
    /// replays barely moves them, since subtracting a measured 14 ms frame gap in
    /// quadrature takes mean spread from 20.51 to only 20.09 ms.
    ///
    /// What rules the band out is identifiability. Sweeping the floor over 0-10 ms
    /// leaves mean `g_timing` on the 20-score fixture set *bit-identical* at
    /// 51.552835, because the counts pin sigma while the fit is free to move skill:
    /// a 2 ms floor at a 16 ms sigma just needs the skill term to shrink 0.78%, which
    /// `skill^-1.7` supplies exactly. So the counts cannot see a small floor at all,
    /// making it as unfittable as [`Self::sigma_ref`]. The window scalar moves
    /// regardless — EZ slides 0.8273 to 0.8123 over that sweep — because it is a
    /// ratio of skills fitted at two different sigmas and quadrature is nonlinear.
    /// A parameter no data constrains that still reprices every score is worse than
    /// one that is merely wrong, so it stays at zero. `sunny::tests::sigma_floor_sweep`
    /// is the harness. Settling it needs saturating scores, where the skill term is
    /// small enough for the floor to dominate.
    ///
    /// **Units, if it is ever set.** A replay measures the sd of the whole mixture;
    /// this is the width of the *core* component. They differ by
    /// `sqrt((1 - lapse_weight) + lapse_weight * lapse_ratio^2)` ≈ 1.27 at the
    /// default shape, so a measured 13 ms of spread means 10 ms here. Reading a
    /// measured sd straight in would overstate it by 27%.
    pub sigma_floor: f64,
    /// The share of notes hit from the wide "lapse" component rather than the
    /// narrow "locked in" one.
    ///
    /// Together with [`Self::lapse_ratio`] this makes the error distribution a
    /// two-component normal mixture rather than a single normal. A single normal
    /// provably cannot describe real scores: measured against 20 live scores, the
    /// observed 320 share runs *above* what a fitted normal predicts while the
    /// observed 100 and 50 shares run above it too — a sharper core and a fatter
    /// tail at the same time. One normal has a single width and must trade one for
    /// the other, so it splits the difference and misses at both ends. On the
    /// worst-fitting no-mod score it underpredicted the 100 share by 19x.
    ///
    /// The mixture separates the two: the core width sets the 320 bulk, while this
    /// weight and the ratio set the tail independently.
    ///
    /// Physically it is the difference between notes hit in the groove and notes hit
    /// while recovering — reading ahead, resetting a hand, coming out of a pattern
    /// change. Unlike [`Self::slip_rate`], which it partly replaces in spirit, this
    /// channel still passes through the hit windows, so it responds to them: a wider
    /// `EZ` window catches lapsed notes that a narrow one would not.
    pub lapse_weight: f64,
    /// How much wider the lapse component is than the core, as a multiple.
    ///
    /// Clamped to at least 1.0 — the lapse component is the wide one by definition,
    /// and letting it go narrower would make the two components trade places and the
    /// fit bimodal.
    pub lapse_ratio: f64,
    /// How much wider a key *release* lands than a key press, as a multiple of the
    /// press spread.
    ///
    /// Only reaches the model through [`ln_sigma_scale`], i.e. only for long notes
    /// judged as a single unit (ScoreV1). At `1.0` a release is as controllable as a
    /// press and a long note carries [`LN_SIGMA_SCALE`]; above it the long note is
    /// wider still.
    ///
    /// **Why it is a free parameter when `sigma_floor` was rejected as one.** A floor
    /// is unidentifiable because the fit absorbs it into skill exactly — it moves
    /// every unit's sigma the same way, and skill can undo that. This does not: it
    /// widens *only* the LN population, so it changes the *ratio* of the two
    /// components within a map, which no amount of skill can reproduce. It is
    /// therefore visible to the counts precisely on the maps that motivate it, and
    /// invisible on rice maps where it should do nothing.
    ///
    /// **Why it must be fitted rather than assumed.** `sqrt(2)` is the `1.0` case and
    /// the fixture set already argues against it: introducing the split at `1.0`
    /// improved median `g_timing` on maps of 30-60% long notes (39.3 to 33.1) but made
    /// it *worse* where long notes dominate (46.0 to 53.3 above 60%). Under-widening
    /// explains exactly that pattern, since the mixture approaches a single wide
    /// population as the LN share approaches one.
    ///
    /// **A single value is known to be insufficient.** Sweeping it wants ~1.75 on maps
    /// of 5-60% long notes and ~1.0 where they dominate, with the aggregate flat because
    /// the two cancel. LN *share* cannot express that, which is why the duration
    /// parameters below exist: this is the floor a long hold tends to, and
    /// [`Self::short_hold_penalty`] is the surcharge a short one pays on top.
    pub release_sigma_ratio: f64,
    /// How much wider a release is on an instantaneous hold than on a very long one, as
    /// a fraction of [`Self::release_sigma_ratio`].
    ///
    /// `0.6` would mean a zero-length hold's release lands 1.6x as wide as a long hold's.
    /// Reaches sigma through [`release_ratio_for_duration`], decaying over
    /// [`Self::short_hold_scale`].
    ///
    /// The mechanism: on a short hold the press motion is still in flight when the
    /// release comes due, so the player never gets to reset and place the release
    /// independently. On a long hold they do, and the release becomes an ordinary act
    /// with only the inherent release penalty left.
    ///
    /// **Zero by default**, which makes the ratio duration-independent and leaves the
    /// shipped model resting on the derived `sqrt(2)` with nothing calibrated in it. The
    /// duration sweep is what should move it.
    pub short_hold_penalty: f64,
    /// The hold duration, in ms, over which [`Self::short_hold_penalty`] decays by
    /// `1/e`.
    ///
    /// Sets what counts as "short". The fixture set's long notes have a median of 100 ms
    /// and a p10 of 50 ms, so a scale in the low hundreds makes the surcharge bite on the
    /// bulk of real long notes while leaving half-second holds alone; a scale of a few
    /// thousand would charge everything alike and be indistinguishable from raising
    /// [`Self::release_sigma_ratio`].
    ///
    /// Only has an effect when the penalty is nonzero, so the default is a plausible
    /// starting point rather than a fitted value.
    pub short_hold_scale: f64,
    /// The per-note probability of a lapse unrelated to timing precision: a
    /// misread, a slipped finger, a dropped input.
    ///
    /// **Zero by default.** Misses are meant to come from the timing distribution
    /// reaching past the MISS boundary, which makes them respond to the things
    /// that should move them: a wider miss window under EZ predicts fewer, and a
    /// difficulty spike predicts more. A flat additive rate responds to neither.
    ///
    /// It is also the wrong functional form for long maps. Independent per-note
    /// lapses make a clean score decay exponentially with length: at `0.005` an SS
    /// on 1300 notes is `0.995^1300` ≈ 0.15%, and on 6358 notes ≈ `1.4e-14`. SS
    /// scores on six-thousand-note maps exist and are not one-in-70-trillion
    /// events, so the length scaling, not just the constant, was wrong.
    ///
    /// Only affects [`expected_counts`] and the distribution it comes from — the
    /// fitting path conditions misses away entirely, so this never influenced
    /// [`skill_for_counts`]. Set it nonzero only to model a population-average
    /// dropped-input rate, and read the result as a population statement rather
    /// than a claim about any one score.
    pub slip_rate: f64,
    /// The mean timing offset, in ms, of a long-note release judgement. Positive
    /// means late.
    ///
    /// Every other field in this struct is a *width*: it scales `sigma`, and a
    /// change in fitted skill can always undo a pure scaling, which is exactly why
    /// [`Self::sigma_floor`] turned out to be unidentifiable. A release is not a
    /// wider press, it is a different motor act — the press is cued by the beat and
    /// timed against it, but the release is cued by the hold *ending*, which the
    /// player tracks less precisely and tends to let run long. That gives it a
    /// systematic lateness a press does not have, and lateness is a shift in the
    /// mean of the error distribution, not a change in its spread.
    ///
    /// **Why this is identifiable where the widths are not.** Skill enters the model
    /// only through `sigma(d, skill)`; it multiplies spread and cannot move a mean.
    /// So a nonzero mean here changes the *shape* of the release population relative
    /// to the press population — pushing PERFECTs to GREATs and GREATs to GOODs on
    /// one side of the window while doing nothing on the other — in a way no
    /// per-score skill fit can absorb by scaling. It is visible to the counts
    /// specifically as an asymmetry that a symmetric width parameter cannot produce.
    ///
    /// **`8.0` is an unfitted starting guess**, chosen as roughly half the PERFECT
    /// window (16 ms in the classic scheme, flat across OD), not a measured value.
    /// It only reaches the model through long-note units — see
    /// [`JudgementUnit::mean_offset`] and [`JudgementUnit::long_note`] — so rice notes
    /// are untouched.
    ///
    /// **Measured, and deliberately left unfitted.** Sweeping it over `0/4/8/16` on the
    /// 143-score multiuser set halves the *reference-side* median fit on maps above 60%
    /// long notes (`g_timing` 66.6 to 34.0 at `16.0`), so the late-release mechanism is
    /// real and the counts can see it. But the *played-side* fit does not move at all
    /// (40.95 at every value, on both that cohort and the low-OD 7K one), and pp is
    /// priced on the played side. The whole gain therefore lands in the denominator of
    /// `window_scalar`'s `played.skill / reference.skill`, which *lowers* pp on exactly
    /// the maps the offset was meant to raise — a ratio artefact, not a modelling error.
    ///
    /// So this value must not be calibrated until pp stops reading a ratio: any number
    /// fitted now is fitted to that artefact. Read the sweep as evidence about the
    /// *mechanism*, not as a reason to pick a constant.
    pub release_mean_offset: f64,
    /// Peak lateness in ms of a press whose column was tapped immediately before it,
    /// decaying with the gap over [`Self::recovery_tau`] toward
    /// [`Self::anticipation_offset`]. Zero disables the whole mechanism.
    ///
    /// The second mean-offset channel, and the one that is *measured* rather than
    /// guessed. The historical calibration paired 285 replays into 629,418 notes and
    /// grouped their timing errors by the state the note's column was in. Per-score
    /// offsets — each group's mean against that same score's own mean, so the player and
    /// the map divide out — trace a clean curve against same-column gap:
    ///
    /// | gap ms | 115 | 145 | 175 | 210 | 255 | 310 | 380 | 470 | 585 | 750 |
    /// |---|---|---|---|---|---|---|---|---|---|---|
    /// | offset | +13.5 | +5.5 | +3.6 | +1.1 | +0.1 | −2.8 | −3.1 | −3.0 | −3.3 | −3.0 |
    ///
    /// `73.12 * exp(-gap / 72.40) - 3.19` is that historical fit, with a weighted RMSE
    /// of 0.73 ms over a 16.5 ms range. `tools/input_state.py` now contains the complete
    /// deterministic fitting step and its regression fixture; the expanded-pool result
    /// is recorded in `docs/input-state-surface-plan.md`. Two regimes, both physical: a
    /// finger that must lift and re-press lands **late** when rushed, and one with time to
    /// spare **anticipates** and lands early. Historical zero crossing at 227 ms.
    ///
    /// **Why a mean and not a width.** The same measurement finds width effects too, but
    /// they mostly vanish once each score is compared against itself, and what survives is
    /// gauge: skill enters only through `sigma`, so it rescales spread exactly and absorbs
    /// any width change. It cannot move a mean. See [`Self::release_mean_offset`] for the
    /// same argument.
    ///
    /// **Why the sign test matters more than the size.** 46 of 46 scores agree on the
    /// direction in the shortest bin, and agreement collapses to 91/181 exactly at the zero
    /// crossing — which is what a real curve does and a step function cannot.
    ///
    /// **The artefact this had to survive.** The replay parser's pairing gives a press the
    /// frontmost note whose GOOD window is still open, so in a pattern tighter than that
    /// window a press meant for the next note is booked against this one, manufacturing
    /// late bias from nothing. Restricting to notes whose predecessor lies beyond that
    /// window shows the artefact inflates the effect about 2.5x (+24.4 vs +9.7 ms) without
    /// causing it, and the curve keeps decaying smoothly to 850 ms — an order of magnitude
    /// past a boundary fixed at 100–140 ms.
    ///
    /// The expanded-pool fitted curve is enabled by default on this experiment branch.
    /// The map-side builder centers these per-score-relative offsets before fitting, so
    /// the curve changes the distribution between input states without introducing an
    /// absolute clock shift. Updating the accepted model constants from a later replay
    /// calibration is a model change and must be checked through the pp A/B gates.
    pub recovery_offset: f64,
    /// The gap in ms over which [`Self::recovery_offset`] decays, `e`-folding.
    ///
    /// 116.68 ms in the expanded-pool fit. Physically the lift-and-repress cycle time,
    /// which is why the value is plausible rather than merely convenient: it is the same
    /// order as the fastest sustained same-column tapping in those maps.
    pub recovery_tau: f64,
    /// Where [`Self::recovery_offset`] decays *to*, in ms, at long gaps. Negative is early.
    ///
    /// −2.517 ms in the expanded-pool fit, and it is not a nuisance term: with a whole beat
    /// of warning players consistently press early, and the plateau is flat from 280 ms
    /// out to 850 ms across 240k notes. Applies to every press with a predecessor, so on
    /// a sparse map it is the only part of this mechanism that acts.
    pub anticipation_offset: f64,
}

impl Default for ErrorModel {
    fn default() -> Self {
        Self {
            sigma_ref: 18.0,
            skill_exponent: 1.7,
            difficulty_floor: 0.6,
            sigma_floor: 0.0,
            lapse_weight: 0.0296,
            lapse_ratio: 3.339,
            // The no-asymmetry floor until the sweep says otherwise, so the shipped
            // default still rests on the derived `sqrt(2)` rather than on a guess.
            release_sigma_ratio: 1.0,
            short_hold_penalty: 0.0,
            short_hold_scale: 120.0,
            slip_rate: 0.0,
            release_mean_offset: 8.0,
            recovery_offset: 20.425,
            recovery_tau: 116.68,
            anticipation_offset: -2.517,
        }
    }
}

impl ErrorModel {
    /// The mean timing offset in ms of a press whose column last saw a press `gap_ms`
    /// earlier. Positive is late.
    ///
    /// `recovery_offset * exp(-gap / recovery_tau) + anticipation_offset`, the curve
    /// measured on replay notes — see [`Self::recovery_offset`] for the historical and
    /// expanded-pool data, fit, and artefact controls.
    ///
    /// A non-finite or negative gap returns the long-gap plateau rather than extrapolating,
    /// and an infinite gap (a column's first note, which has no predecessor to recover
    /// from) returns zero: there is no prior press to be late against, so the note carries
    /// no offset from this mechanism at all.
    pub fn recovery_mean_offset(&self, gap_ms: f64) -> f64 {
        if self.recovery_offset == 0.0 && self.anticipation_offset == 0.0 {
            return 0.0;
        }

        if !gap_ms.is_finite() {
            return 0.0;
        }

        if gap_ms <= 0.0 {
            return self.recovery_offset + self.anticipation_offset;
        }

        let tau = if self.recovery_tau > 0.0 {
            self.recovery_tau
        } else {
            return self.anticipation_offset;
        };

        self.recovery_offset * (-gap_ms / tau).exp() + self.anticipation_offset
    }

    /// The timing error standard deviation, in ms, for local difficulty
    /// `difficulty` at player skill `skill`.
    ///
    /// Returns [`f64::INFINITY`] for non-positive or NaN skill, i.e. a guaranteed
    /// miss.
    pub fn sigma(&self, difficulty: f64, skill: f64) -> f64 {
        // NaN is treated as unplayable rather than propagated, so a bad input
        // cannot poison a whole judgement distribution.
        if skill.is_nan() || skill <= 0.0 {
            return f64::INFINITY;
        }

        let ratio = (difficulty.max(0.0) + self.difficulty_floor) / skill;
        let skill_term = self.sigma_ref * ratio.powf(self.skill_exponent);

        // Quadrature, not `max`: the floor is an independent noise source (input
        // latency jitter, scan-out timing, the hand itself) so it adds in variance,
        // and the smooth join keeps the curve differentiable for the fit.
        self.sigma_floor.max(0.0).hypot(skill_term)
    }

    /// The probability that the error on a single note exceeds `bound` in absolute
    /// value, under the two-component mixture.
    ///
    /// This is the only place the mixture is applied. Both components share the same
    /// [`Self::sigma`] scale — the lapse one widened by [`Self::lapse_ratio`] — so a
    /// change in skill moves the whole distribution together and the *shape* stays
    /// fixed. That separation is what lets the shape be calibrated once against real
    /// scores while skill remains the only per-score free quantity.
    fn exceedance(&self, bound: f64, sigma: f64) -> f64 {
        let weight = self.lapse_weight.clamp(0.0, 1.0);

        if weight <= 0.0 {
            return tail(bound, sigma);
        }

        // The lapse component is the wide one by definition; a ratio below 1 would
        // swap the roles and give the fit two equivalent optima.
        let ratio = self.lapse_ratio.max(1.0);

        (1.0 - weight) * tail(bound, sigma) + weight * tail(bound, sigma * ratio)
    }

    /// As [`Self::exceedance`], but for an error distribution shifted by a mean
    /// `mu` rather than centred at zero: the probability that a release lands
    /// outside `[-bound, +bound]` when its offset is drawn from `N(mu, sigma)`.
    ///
    /// **The algebra.** Write `Z = X - mu`, so `Z ~ N(0, sigma)` and
    /// [`one_sided_tail`] gives `P(Z > x)` directly. Then
    ///
    /// ```text
    /// P(X > bound)  = P(Z > bound - mu)         = one_sided_tail(bound - mu, sigma)
    /// P(X < -bound) = P(Z < -bound - mu)
    ///               = P(-Z > bound + mu)         (negate both sides)
    ///               = P(Z > bound + mu)           (Z is symmetric about 0)
    ///               = one_sided_tail(bound + mu, sigma)
    /// ```
    ///
    /// so the two-sided tail is `one_sided_tail(bound - mu, sigma) + one_sided_tail(bound
    /// + mu, sigma)`. At `mu = 0` both terms equal `one_sided_tail(bound, sigma)`, and
    /// their sum is `2 * one_sided_tail(bound, sigma) = tail(bound, sigma)` by
    /// [`one_sided_tail`]'s own definition — exactly [`Self::exceedance`]'s zero-mean
    /// case. Callers take that equivalence on faith and call [`Self::exceedance`]
    /// directly when `mu == 0.0`, rather than routing through here, so that the
    /// zero-offset path is bit-for-bit whatever it was before this function existed.
    ///
    /// The offset is applied to *both* mixture components with the same `mu`: a lapse
    /// is still a release, cued by the same hold ending, so there is no reason its
    /// mean should differ from the core component's.
    fn exceedance_with_offset(&self, bound: f64, sigma: f64, mu: f64) -> f64 {
        if mu == 0.0 {
            return self.exceedance(bound, sigma);
        }

        // Mirrors `tail`'s own guard: a non-positive bound is a zero-width window,
        // which nothing lands inside of, however the distribution is shifted. Left
        // unguarded, the two-term sum below can exceed 1 for a negative bound, which
        // never occurs on a real hit window but is worth clamping defensively.
        if bound <= 0.0 {
            return 1.0;
        }

        let two_sided = |s: f64| one_sided_tail(bound - mu, s) + one_sided_tail(bound + mu, s);

        let weight = self.lapse_weight.clamp(0.0, 1.0);

        if weight <= 0.0 {
            return two_sided(sigma);
        }

        let ratio = self.lapse_ratio.max(1.0);

        (1.0 - weight) * two_sided(sigma) + weight * two_sided(sigma * ratio)
    }
}

/// The probability of each judgement for a single hit.
///
/// Entries are indexed by [`ManiaJudgement::ALL`] and sum to 1.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct JudgementProbabilities([f64; 6]);

impl JudgementProbabilities {
    /// The probability of a single judgement.
    pub fn get(&self, judgement: ManiaJudgement) -> f64 {
        self.0[judgement as usize]
    }

    /// The probabilities in [`ManiaJudgement::ALL`] order.
    pub fn as_array(&self) -> [f64; 6] {
        self.0
    }

    /// The accuracy this distribution implies under the 305-weighted scheme used
    /// by [`crate::mania::sunny::custom_accuracy`].
    pub fn custom_accuracy(&self) -> f64 {
        const WEIGHTS: [f64; 6] = [305.0, 300.0, 200.0, 100.0, 50.0, 0.0];

        let weighted: f64 = self
            .0
            .iter()
            .zip(WEIGHTS)
            .map(|(probability, weight)| probability * weight)
            .sum();

        weighted / 305.0
    }
}

/// The probability that a zero-mean normal with standard deviation `sigma`
/// produces an absolute error *greater* than `bound`.
///
/// This is the complement of `erf(bound / (sigma * sqrt(2)))`, but computed
/// directly rather than by subtraction. That matters: the interesting part of a
/// high-skill score lives in this tail, and `1 - erf(x)` collapses to exactly
/// zero for `x` past about 6, which would flatten the surface for every skilled
/// player. Working in the tail keeps it meaningful down to ~1e-300.
fn tail(bound: f64, sigma: f64) -> f64 {
    if bound <= 0.0 {
        return 1.0;
    }

    if bound.is_infinite() {
        return 0.0;
    }

    if sigma.is_infinite() {
        return 1.0;
    }

    // Zero spread means perfect timing, so no mass escapes any positive bound.
    // NaN lands here too and is treated the same way, rather than propagating.
    if sigma.is_nan() || sigma <= 0.0 {
        return 0.0;
    }

    erfc(bound / (sigma * std::f64::consts::SQRT_2))
}

/// The probability that a zero-mean normal with standard deviation `sigma`
/// exceeds `x` — not in absolute value, unlike [`tail`].
///
/// `x` may be negative. This is needed by
/// [`ErrorModel::exceedance_with_offset`], where a window bound shifted by a
/// release's mean offset can land on either side of zero even though the
/// window itself never does. [`crate::mania::sunny_accuracy::erfc`] is a true
/// complementary error function (correct for negative arguments, not just
/// mirrored around a `bound <= 0` guard the way [`tail`] is), so this is a
/// thin wrapper rather than a second approximation:
///
/// ```text
/// P(Z > x) = 1 - Phi(x / sigma) = 0.5 * erfc(x / (sigma * sqrt(2)))
/// ```
///
/// For `x > 0` and finite positive `sigma`, `tail(x, sigma) == 2.0 *
/// one_sided_tail(x, sigma)`: the two-sided tail is exactly the sum of the two
/// one-sided tails at `x` and `-x`, and by `erfc`'s symmetry those two terms
/// are equal.
fn one_sided_tail(x: f64, sigma: f64) -> f64 {
    if x.is_infinite() {
        return if x > 0.0 { 0.0 } else { 1.0 };
    }

    if sigma.is_infinite() {
        // An infinitely wide distribution places no more than half its mass on
        // either side of any finite point — the same convention `tail` uses,
        // where the two-sided version of this (`1.0`) means "assume everything
        // escapes", i.e. certain miss.
        return 0.5;
    }

    // Zero (or degenerate) spread means a point mass at 0: certain to be found
    // on whichever side of `x` contains the origin.
    if sigma.is_nan() || sigma <= 0.0 {
        return if x > 0.0 {
            0.0
        } else if x < 0.0 {
            1.0
        } else {
            0.5
        };
    }

    0.5 * erfc(x / (sigma * std::f64::consts::SQRT_2))
}

/// The complementary error function, via the Numerical Recipes rational
/// approximation.
///
/// Fractional error is below `1.2e-7` *everywhere*, including deep in the tail —
/// unlike the more common Abramowitz & Stegun 7.1.26 form, whose error is
/// absolute and therefore destroys the tail entirely.
pub(crate) fn erfc(x: f64) -> f64 {
    let z = x.abs();
    let t = 1.0 / (1.0 + 0.5 * z);

    let poly = -1.265_512_23
        + t * (1.000_023_68
            + t * (0.374_091_96
                + t * (0.096_784_18
                    + t * (-0.186_288_06
                        + t * (0.278_868_07
                            + t * (-1.135_203_98
                                + t * (1.488_515_87 + t * (-0.822_152_23 + t * 0.170_872_77))))))));

    let value = t * (-z * z + poly).exp();

    if x >= 0.0 { value } else { 2.0 - value }
}

/// The judgement distribution for a single hit of local difficulty `difficulty`
/// at player skill `skill`.
pub fn judgement_probabilities(
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    difficulty: f64,
    skill: f64,
) -> JudgementProbabilities {
    judgement_probabilities_scaled(windows, model, difficulty, skill, 1.0, 0.0)
}

/// As [`judgement_probabilities`], with the unit's timing spread multiplied by
/// `sigma_scale` and its error distribution's mean shifted by `mean_offset` ms
/// (positive = late).
///
/// The scale exists for judgement units whose spread differs from a plain note's
/// by a factor *derived from the map*, not fitted: a ScoreV1 long note, whose head
/// and release offsets are summed into a single judgement, is the motivating case
/// at `sqrt(2)`. Because it multiplies sigma rather than replacing it, the unit
/// still tracks `difficulty / skill`, so nothing becomes unrepresentable at high
/// skill the way a fixed sigma floor would.
///
/// `mean_offset` is unrelated to `sigma_scale` and is *not* multiplied by it: a
/// release's lateness is a fixed number of milliseconds
/// ([`ErrorModel::release_mean_offset`]), not a fraction of its spread, so the two
/// travel independently — see [`JudgementUnit::mean_offset`].
pub fn judgement_probabilities_scaled(
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    difficulty: f64,
    skill: f64,
    sigma_scale: f64,
    mean_offset: f64,
) -> JudgementProbabilities {
    // A non-positive or NaN scale would silently turn a hard unit into a free one,
    // so it is treated as "no scaling" rather than propagated.
    let scale = if sigma_scale.is_finite() && sigma_scale > 0.0 {
        sigma_scale
    } else {
        1.0
    };

    // A non-finite offset is treated as "no shift" rather than propagated, for the
    // same reason: it must not silently turn into a NaN that poisons every band.
    let mu = if mean_offset.is_finite() {
        mean_offset
    } else {
        0.0
    };

    let sigma = model.sigma(difficulty, skill) * scale;

    let mut probabilities = [0.0; 6];
    // Mass still outside every window considered so far, starting with all of it.
    let mut remaining = 1.0;

    for judgement in ManiaJudgement::ALL {
        let (_, upper) = windows.band(judgement);
        let outside = model
            .exceedance_with_offset(upper, sigma, mu)
            .min(remaining);
        // Bands are nested, so each judgement claims the mass that falls inside
        // its window but outside every tighter one. Differencing tails rather
        // than cumulatives keeps the sub-PERFECT judgements accurate at high
        // skill, where they are the only thing distinguishing one score from
        // another.
        probabilities[judgement as usize] = remaining - outside;
        remaining = outside;
    }

    // Optionally mix in the lapse channel. Zero at the default, so the miss
    // probability above is entirely the timing distribution's tail past the MISS
    // boundary — which is what makes it respond to the miss window and to local
    // difficulty rather than sitting at a flat `n * slip_rate`.
    //
    // When set nonzero it is a population average and should be read as one: per
    // note `1 - slip_rate` of the mass stays where the timing model put it, so the
    // mean PERFECT count over `n` notes is `n * (1 - slip_rate)`, and an all-PERFECT
    // score costs a factor of `(1 - slip_rate)^n`. That factor is what made the
    // nonzero default untenable on long maps.
    let slip = model.slip_rate.clamp(0.0, 1.0);

    if slip > 0.0 {
        for probability in &mut probabilities {
            *probability *= 1.0 - slip;
        }

        probabilities[ManiaJudgement::Miss as usize] += slip;
    }

    JudgementProbabilities(probabilities)
}

/// The expected judgement counts over a set of judgement units.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct ExpectedCounts([f64; 6]);

impl ExpectedCounts {
    /// The expected count of a single judgement.
    pub fn get(&self, judgement: ManiaJudgement) -> f64 {
        self.0[judgement as usize]
    }

    /// The counts in [`ManiaJudgement::ALL`] order.
    pub fn as_array(&self) -> [f64; 6] {
        self.0
    }

    /// The total number of judgement units, i.e. the sum of all counts.
    pub fn total(&self) -> f64 {
        self.0.iter().sum()
    }

    /// The 305-weighted accuracy these counts imply.
    pub fn custom_accuracy(&self) -> f64 {
        const WEIGHTS: [f64; 6] = [305.0, 300.0, 200.0, 100.0, 50.0, 0.0];

        let total = self.total();

        if total <= 0.0 {
            return 0.0;
        }

        let weighted: f64 = self
            .0
            .iter()
            .zip(WEIGHTS)
            .map(|(count, weight)| count * weight)
            .sum();

        weighted / (total * 305.0)
    }

    /// Round to whole notes, preserving the total exactly.
    ///
    /// Useful for presenting a surface sample as a plausible score. Any rounding
    /// residue lands on the judgement with the largest fractional part.
    pub fn round_to_hits(&self, total_units: u32) -> [u32; 6] {
        let mut counts = [0u32; 6];
        let mut remainders = [(0.0, 0usize); 6];
        let mut assigned = 0u32;

        for (idx, &count) in self.0.iter().enumerate() {
            let floored = count.max(0.0).floor();
            counts[idx] = floored as u32;
            assigned += counts[idx];
            remainders[idx] = (count.max(0.0) - floored, idx);
        }

        remainders.sort_by(|a, b| b.0.total_cmp(&a.0));

        let mut leftover = total_units.saturating_sub(assigned);

        for &(_, idx) in remainders.iter().cycle().take(6) {
            if leftover == 0 {
                break;
            }

            counts[idx] += 1;
            leftover -= 1;
        }

        counts
    }
}

/// A single unit that receives a judgement, with its local difficulty.
///
/// One per note in classic scoring; LN heads and tails are separate units when
/// they are judged separately.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct JudgementUnit {
    /// Local difficulty in star-rating units.
    pub difficulty: f64,
    /// How many judgements this unit stands for. Lets identical units collapse
    /// into one entry, which keeps the surface cheap on dense maps.
    pub weight: f64,
    /// A multiplier on this unit's timing spread, for units that are structurally
    /// wider than a plain note at the same difficulty.
    ///
    /// One for an ordinary note. [`LN_SIGMA_SCALE`] for a ScoreV1 long note, where
    /// two offsets are summed into one judgement so their variances add. Always
    /// read off the map's own structure, never fitted to the score — see
    /// [`Self::long_note`].
    pub sigma_scale: f64,
    /// A shift, in ms, applied to this unit's error distribution mean (positive =
    /// late).
    ///
    /// Zero for an ordinary note. [`ErrorModel::release_mean_offset`] for a ScoreV1
    /// long note — see [`Self::long_note`]. Unlike [`Self::sigma_scale`] this is
    /// never itself scaled by `sigma_scale`: a release's lateness is a fixed
    /// millisecond offset, not a fraction of the unit's spread, so a duration bucket
    /// that widens `sigma_scale` does not also widen this.
    pub mean_offset: f64,
    /// A structural mean shift that fades with timing spread, reaching its full value
    /// at `sigma_ref` and tending to zero as the player approaches perfect precision.
    /// Used for recoverable press bias; unlike a fixed release offset it must not make
    /// an all-PERFECT score mathematically unreachable.
    pub fading_mean_offset: f64,
}

/// How much wider a ScoreV1 long note's effective timing spread is than a plain
/// note's when a release is exactly as controllable as a press: `sqrt(2)`.
///
/// Under ScoreV1 an LN head and its release form a *single* judgement, graded on the
/// sum of the two offsets. Independent offsets of equal spread `sigma` sum to spread
/// `sigma * sqrt(2)`, so the same physical precision converts fewer PERFECTs on a
/// long note than on a rice note. Under ScoreV2 the two are judged separately and
/// this does not apply; the caller decides which regime the score was played under.
///
/// **This is a floor, not the expected value.** It assumes the release offset has the
/// same spread as the press, which players consistently report is false — letting go
/// of a key is harder to place than hitting it. With a release spread `k` times the
/// press spread the combined scale is [`ln_sigma_scale`]'s `sqrt(1 + k^2)`, and this
/// constant is the `k = 1` case. Keep it as the reference point that involves no
/// calibration at all, and see [`ErrorModel::release_sigma_ratio`] for the fitted
/// asymmetry.
pub const LN_SIGMA_SCALE: f64 = std::f64::consts::SQRT_2;

/// How many long-note duration buckets the judgement model distinguishes.
///
/// Lives here rather than in `sunny` because it is a property of the error model — the
/// map layer only counts notes into whatever buckets the model asks for. The edges
/// themselves are `sunny::LN_DURATION_EDGES`, since those are in map time.
///
/// Eight rather than five, chosen by measurement: at five bins the spread of the
/// spread-multiplier *within* a bin reached 32%, and
/// `sunny::tests::ln_binning_error_stays_small` measured a 4.5% worst-case error in
/// fitted skill against evaluating every long note at its own duration. Eight brings
/// that under the 2% bar. The cost is linear and trivial — a fit walks one unit per
/// occupied bin, so at most nine.
pub const LN_DURATION_BUCKETS: usize = 8;

/// The timing-spread multiplier for a ScoreV1 long note whose release is
/// `release_ratio` times as wide as its press: `sqrt(1 + release_ratio^2)`.
///
/// The head and release offsets are summed into one judgement, so their variances
/// add. Writing the press spread as `sigma` and the release as `release_ratio *
/// sigma`, the sum has spread `sigma * sqrt(1 + release_ratio^2)`. At
/// `release_ratio = 1` this is exactly [`LN_SIGMA_SCALE`]; at 1.5 it is 1.80, and at
/// 2 it is 2.24.
///
/// A ratio below 1 would say releases are *easier* to place than presses, which no
/// account of mania playing supports, so it is clamped away.
pub fn ln_sigma_scale(release_ratio: f64) -> f64 {
    if !release_ratio.is_finite() {
        return LN_SIGMA_SCALE;
    }

    let ratio = release_ratio.max(1.0);

    (1.0 + ratio * ratio).sqrt()
}

/// How much wider a release lands than a press, for a hold of `duration` ms.
///
/// Two effects, both pushing the same way:
///
/// - **A release is inherently harder to place than a press.** There is no impact to
///   time against, and the finger is moving off the key rather than onto it. That is the
///   `release_ratio` floor, applying at any duration.
/// - **A shorter hold is harder still.** The press motion is not finished when the
///   release is already due, so the player has no chance to reset — the release is
///   placed relative to a press that is itself still in flight. As the hold lengthens
///   the release becomes its own independent act and this surcharge decays away.
///
/// Modelled as
///
/// ```text
/// ratio(t) = release_ratio * (1 + short_hold_penalty * exp(-t / short_hold_scale))
/// ```
///
/// so `short_hold_scale` is the duration over which the surcharge decays by `1/e` and
/// `short_hold_penalty` is its size at zero duration. A long hold tends to
/// `release_ratio`, recovering the duration-independent model exactly.
///
/// Exponential rather than a power law or a hard cutoff, for three reasons: it is
/// monotone and finite at `t = 0` where a power law diverges, it has one obvious scale
/// parameter to fit instead of an exponent plus an offset, and it makes the "long holds
/// are just independent releases" limit exact rather than asymptotic-in-principle.
///
/// Continuous in duration by design. An earlier attempt bucketed duration into five
/// *semantic* categories with a free correlation each, which is five parameters for one
/// monotone effect and unidentifiable on 119 scores.
pub fn release_ratio_for_duration(model: &ErrorModel, duration: f64) -> f64 {
    let base = model.release_sigma_ratio.max(1.0);

    if !duration.is_finite() || duration <= 0.0 {
        // A non-positive duration is not a hold at all; charge the full surcharge
        // rather than silently treating it as an infinitely long one.
        return base * (1.0 + model.short_hold_penalty.max(0.0));
    }

    let penalty = model.short_hold_penalty.max(0.0);

    if penalty <= 0.0 {
        return base;
    }

    let scale = model.short_hold_scale;

    // A non-positive scale means "no duration dependence": the surcharge would decay
    // instantly, which is the same as not having one.
    if !scale.is_finite() || scale <= 0.0 {
        return base;
    }

    base * (1.0 + penalty * (-duration / scale).exp())
}

/// The timing-spread multiplier for a ScoreV1 long note of `duration` ms.
///
/// Composes [`release_ratio_for_duration`] with [`ln_sigma_scale`]: the duration sets
/// how much wider the release is, and the variances then add because the two offsets are
/// summed into one judgement.
pub fn ln_sigma_scale_for_duration(model: &ErrorModel, duration: f64) -> f64 {
    ln_sigma_scale(release_ratio_for_duration(model, duration))
}

impl JudgementUnit {
    /// A single judgement of the given local difficulty.
    pub fn new(difficulty: f64) -> Self {
        Self {
            difficulty,
            weight: 1.0,
            sigma_scale: 1.0,
            mean_offset: 0.0,
            fading_mean_offset: 0.0,
        }
    }

    /// `count` judgements sharing the same local difficulty.
    pub fn repeated(difficulty: f64, count: f64) -> Self {
        Self {
            difficulty,
            weight: count,
            sigma_scale: 1.0,
            mean_offset: 0.0,
            fading_mean_offset: 0.0,
        }
    }

    /// `count` ScoreV1 long-note judgements of the given local difficulty, widened
    /// for the model's release asymmetry and shifted by its release lateness.
    ///
    /// Takes the model rather than bare numbers so neither the release ratio nor the
    /// mean offset can drift apart from the ones the fit is using. `duration_ms` is
    /// how long the hold lasts in map time, which decides how much of the
    /// short-hold surcharge the *width* pays — [`ErrorModel::release_mean_offset`]
    /// is not duration-dependent, so it applies in full regardless of hold length.
    pub fn long_note(difficulty: f64, count: f64, model: &ErrorModel, duration_ms: f64) -> Self {
        Self {
            difficulty,
            weight: count,
            sigma_scale: ln_sigma_scale_for_duration(model, duration_ms),
            mean_offset: model.release_mean_offset,
            fading_mean_offset: 0.0,
        }
    }

    /// The same unit with its spread multiplied by `scale`.
    pub fn with_sigma_scale(self, scale: f64) -> Self {
        Self {
            sigma_scale: scale,
            ..self
        }
    }
}

/// Expected judgement counts across every unit at player skill `skill`.
pub fn expected_counts(
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    skill: f64,
) -> ExpectedCounts {
    let mut totals = [0.0; 6];

    for unit in units {
        let sigma = model.sigma(unit.difficulty, skill) * unit.sigma_scale;
        let fade = if model.sigma_ref.is_finite() && model.sigma_ref > 0.0 {
            (sigma / model.sigma_ref).clamp(0.0, 1.0)
        } else {
            0.0
        };
        let probabilities = judgement_probabilities_scaled(
            windows,
            model,
            unit.difficulty,
            skill,
            unit.sigma_scale,
            unit.mean_offset + unit.fading_mean_offset * fade,
        );

        for judgement in ManiaJudgement::ALL {
            totals[judgement as usize] += unit.weight * probabilities.get(judgement);
        }
    }

    ExpectedCounts(totals)
}

// ---------------------------------------------------------------------------
// Inversion: score -> skill
// ---------------------------------------------------------------------------

/// The skill range the inversions search, in star-rating units. The upper bound
/// is far above any real map's difficulty so that near-perfect scores still
/// bracket.
const SKILL_MIN: f64 = 1e-3;
const SKILL_MAX: f64 = 1e4;

/// Below this skill the conditional fit stops resolving anything, and a returned
/// value should be read as "at most this skilled" rather than as a measurement.
///
/// Not a clamp — [`skill_for_counts`] still returns whatever the likelihood peak
/// says, since clamping would hide the situation rather than report it. The band
/// shares the fit reads converge on fixed window-width ratios as sigma grows past
/// the miss boundary, so scores that differ only in miss count become
/// indistinguishable. On an OD9 map the convergence is essentially complete by
/// skill 0.3, where sigma is ~2600 ms; 1.0 leaves a comfortable margin and still
/// sits far below any score that earns pp.
///
/// Use [`FitQuality::is_identifiable`] rather than comparing against this
/// directly.
pub const SKILL_IDENTIFIABLE_MIN: f64 = 1.0;

/// Above roughly this multiple of a map's difficulty the surface saturates: every
/// note is a near-certain 320, accuracy is exactly 1.0, and the likelihood stops
/// responding to skill.
///
/// Expressed as a ratio rather than an absolute skill because the ceiling scales
/// with the map. Measured on OD9 the saturation point sits at 4.8x difficulty on a
/// 2-star map and 3.7x on a 20-star one, so a single ratio is a slight
/// simplification; the value here is the conservative end. EZ lowers it further by
/// widening the windows.
///
/// This is why a fitted skill on an SS is a lower bound. Harmless for pp — every
/// score up there is an SS and should score the same — but it matters when
/// calibrating, since such scores cannot pin a skill value.
pub const SKILL_SATURATION_RATIO: f64 = 3.7;

/// How many bisection steps the accuracy inversion takes. 60 halvings of a
/// ratio-space bracket puts the result well inside f64 noise.
const BISECT_STEPS: u32 = 60;

/// The skill level at which the surface produces the given 305-weighted
/// accuracy.
///
/// Accuracy rises monotonically with skill, so this bisects. Returns
/// [`SKILL_MAX`] for an accuracy the surface cannot reach even at the top of the
/// bracket, which is what a genuine 100% on a trivial map does.
pub fn skill_for_accuracy(
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    target_accuracy: f64,
) -> f64 {
    if units.is_empty() {
        return SKILL_MIN;
    }

    let accuracy_at = |skill: f64| expected_counts(units, windows, model, skill).custom_accuracy();

    if accuracy_at(SKILL_MIN) >= target_accuracy {
        return SKILL_MIN;
    }

    if accuracy_at(SKILL_MAX) <= target_accuracy {
        return SKILL_MAX;
    }

    let mut low = SKILL_MIN;
    let mut high = SKILL_MAX;

    for _ in 0..BISECT_STEPS {
        // Geometric midpoint: skill spans orders of magnitude, so halving the
        // ratio converges evenly across the whole range.
        let mid = (low * high).sqrt();

        if accuracy_at(mid) < target_accuracy {
            low = mid;
        } else {
            high = mid;
        }
    }

    (low * high).sqrt()
}

/// The log-likelihood of observing `counts` judgements at player skill `skill`.
///
/// Multinomial up to the constant coefficient, which does not depend on skill and
/// so drops out of any maximization. Used by [`skill_for_counts`] and available
/// for fitting [`ErrorModel`] against real scores.
pub fn log_likelihood(
    counts: &[u32; 6],
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    skill: f64,
) -> f64 {
    let expected = expected_counts(units, windows, model, skill);

    // Condition on the note having been hit: the miss channel is dropped and the
    // five timing bands are renormalised among themselves.
    //
    // For any score a player would care about, misses carry almost no information
    // about timing precision. On a map of even difficulty the timing model predicts
    // none at all above skill ~4, so every miss in such a score comes from
    // somewhere the model does not describe — a lag spike, a misread, a hand off
    // the keys. Scoring the miss count would make the estimate hostage to whichever
    // fixed rate `slip_rate` happens to hold, which is a population average and
    // wrong for any individual score.
    //
    // Conditioning also removes the awkwardness of a model that insists on
    // ~n * slip_rate misses for a score that has none. Misses are still fully
    // accounted for on the *scoring* side, where they belong: they lower accuracy
    // and break combo directly. What they should not do is corrupt the inference
    // about how precisely the player hit the notes they did hit.
    //
    // The cost is at the very bottom of the scale, and it is why
    // `SKILL_IDENTIFIABLE_MIN` exists. As skill falls the five band *shares*
    // converge on the fixed ratios set by the window widths — 0.193 / 0.241 /
    // 0.265 / 0.169 / 0.132 on OD9 — because a sigma of hundreds of ms is
    // effectively uniform across a 161 ms line. Two scores with the same hit
    // spread and wildly different miss counts then have identical conditional
    // likelihoods, and the fit cannot separate them: it reports SKILL_MIN for a
    // total beginner regardless. That is a genuine loss of resolution, but only
    // below where any pp is awarded (sunny's `performance_proportion` is zero at
    // or below 80% accuracy), and it is a flat floor rather than a wrong ordering.
    // See the module docs for why the trade goes this way.
    let timing_total = expected.total() - expected.get(ManiaJudgement::Miss);

    if timing_total <= 0.0 {
        return f64::NEG_INFINITY;
    }

    let mut sum = 0.0;

    for judgement in ManiaJudgement::ALL {
        if judgement == ManiaJudgement::Miss {
            continue;
        }

        let observed = f64::from(counts[judgement as usize]);

        if observed == 0.0 {
            continue;
        }

        // Guard the log against a judgement the surface says is impossible but
        // the score contains anyway; a floor keeps such a score merely very
        // unlikely rather than un-scoreable.
        let probability = (expected.get(judgement) / timing_total).max(1e-12);

        sum += observed * probability.ln();
    }

    sum
}

/// How many points the coarse scan samples across the skill bracket.
///
/// The likelihood is not usable for a naive bracket search: both ends are flat
/// plateaus, because once a judgement's probability underflows it is pinned to the
/// `1e-12` floor and the likelihood stops responding to skill. A derivative-free
/// bracket search started on a plateau cannot tell which way is uphill and walks
/// to the edge. Scanning first locates the peak, so the refinement below always
/// starts on a genuine slope. 256 points over 7 orders of magnitude puts samples
/// ~7% apart in skill, far finer than any real peak.
const SCAN_POINTS: usize = 256;

/// How many golden-section iterations refine the scanned peak. Each shrinks the
/// bracket by ~0.618, so 100 steps take the two-point-wide bracket to f64 noise.
const GOLDEN_STEPS: u32 = 100;

/// The skill level that best explains the full judgement vector.
///
/// Uses the whole vector rather than just accuracy, so it can tell a score that
/// traded 320s for 300s apart from one that dropped notes entirely.
///
/// Note that above roughly 25 skill units on a typical map every note is a near
/// certain 320, so the likelihood flattens and the returned value should be read
/// as "at least this skilled" rather than a point estimate.
pub fn skill_for_counts(
    counts: &[u32; 6],
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
) -> f64 {
    if units.is_empty() || counts.iter().all(|&count| count == 0) {
        return SKILL_MIN;
    }

    let evaluate = |log_skill: f64| log_likelihood(counts, units, windows, model, log_skill.exp());

    let scan_low = SKILL_MIN.ln();
    let scan_high = SKILL_MAX.ln();
    let step = (scan_high - scan_low) / SCAN_POINTS as f64;

    let mut best_value = f64::NEG_INFINITY;
    let mut best_index = 0;

    for index in 0..=SCAN_POINTS {
        let value = evaluate(scan_low + step * index as f64);

        // A strict improvement margin makes ties resolve to the lowest skill,
        // so a saturated score reports where it first became explicable rather
        // than an arbitrary point out on the plateau.
        if value > best_value + 1e-9 {
            best_value = value;
            best_index = index;
        }
    }

    // Golden-section refinement within one scan step either side of the peak.
    const INV_PHI: f64 = 0.618_033_988_749_895;

    let mut low = scan_low + step * best_index.saturating_sub(1) as f64;
    let mut high = scan_low + step * (best_index + 1).min(SCAN_POINTS) as f64;

    let mut c = high - (high - low) * INV_PHI;
    let mut d = low + (high - low) * INV_PHI;
    let mut fc = evaluate(c);
    let mut fd = evaluate(d);

    for _ in 0..GOLDEN_STEPS {
        if fc > fd {
            high = d;
            d = c;
            fd = fc;
            c = high - (high - low) * INV_PHI;
            fc = evaluate(c);
        } else {
            low = c;
            c = d;
            fc = fd;
            d = low + (high - low) * INV_PHI;
            fd = evaluate(d);
        }
    }

    ((low + high) / 2.0).exp()
}

// ---------------------------------------------------------------------------
// Goodness of fit
// ---------------------------------------------------------------------------

/// How well a score's judgement vector matches what the surface predicts.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FitQuality {
    /// The skill the fit settled on.
    pub skill: f64,
    /// The G-test statistic: `2 * sum(observed * ln(observed / expected))`.
    ///
    /// Zero means the counts match the surface exactly. Under the model this is
    /// roughly chi-squared with 4 degrees of freedom (six judgements, less one for
    /// the total and one for the fitted skill), so values past ~10 are unusual and
    /// values in the hundreds mean the score does not resemble anything the
    /// surface can produce.
    ///
    /// It does not grow with map length — simulated well-behaved scores average ~4
    /// whether the map has 500 notes or 10000 — so one cutoff would work across
    /// every map size. It is nonetheless the wrong figure to threshold on, because
    /// it mixes two failures with opposite consequences; see [`Self::g_timing`].
    pub g_statistic: f64,
    /// The same statistic computed over the five timing judgements only, with the
    /// miss channel excluded and the remainder rescaled to the observed timing
    /// total.
    ///
    /// This is the figure [`Self::is_plausible`] keys off, and the reason is that
    /// the two ways a score departs from the surface have opposite implications for
    /// the skill estimate:
    ///
    /// - A *shape* error spread across the timing bands — most commonly a constant
    ///   audio or input offset, which pushes mass out of 320 and into 300/200
    ///   symmetrically — means the fit's central assumption is wrong, and the skill
    ///   it returns is biased low by a lot. A 20 ms offset costs ~25% of estimated
    ///   skill. This is what should be flagged.
    /// - An excess of *misses* — a lag spike, a dropped input, a hand off the keys
    ///   — leaves the timing bands untouched, and the fit ignores it by construction
    ///   since [`log_likelihood`] conditions misses away. The skill estimate barely
    ///   moves: 200 dropped notes out of 1300 shift it by 4%. Flagging this would
    ///   reject ordinary scores while telling us nothing about whether the skill
    ///   number is trustworthy.
    ///
    /// Since the miss channel's expected count is small, it dominates
    /// `g_statistic` as soon as it is exceeded at all — 20 misses against 6.5
    /// predicted contributes 45 on its own. Thresholding the combined figure
    /// therefore rejects scores whose skill estimate is in fact fine.
    ///
    /// This is also the statistic that matches what was actually fitted, since the
    /// fit maximises the conditional likelihood over exactly these five bands.
    pub g_timing: f64,
    /// Observed misses beyond what the surface predicted, or zero if the score has
    /// no more than expected.
    ///
    /// Reported in counts rather than as a rate because that is the form in which
    /// it can be sanity-checked against a map: "34 misses more than expected on a
    /// 1300-note map" is a claim about a specific score, whereas the equivalent
    /// ratio hides how many notes it rests on. Deliberately not part of
    /// [`Self::is_plausible`] — a score with dropped notes is a worse score, which
    /// the accuracy already reflects, not an unexplainable one.
    pub excess_misses: f64,
    /// `g_statistic` divided by the number of judgements: the average
    /// contribution per note.
    ///
    /// Do *not* use this as the plausibility threshold. Because `g_statistic` is
    /// already length-independent, dividing by note count makes this fall as
    /// `1/n`, so a fixed cutoff on it would be far stricter on short maps than long
    /// ones. It is reported because it answers a different question — how badly the
    /// typical note is mispredicted — which is the useful figure when deciding
    /// whether a large `g_statistic` reflects a systematic shape error or a handful
    /// of anomalous notes.
    pub g_per_judgement: f64,
}

impl FitQuality {
    /// Whether the score's *timing shape* is broadly consistent with the surface,
    /// and so whether [`Self::skill`] can be trusted.
    ///
    /// Keys off [`Self::g_timing`], not [`Self::g_statistic`], so that dropped
    /// notes do not read as an unexplainable score. The threshold is deliberately
    /// loose: this is meant to catch scores the model cannot describe at all, not
    /// to police ordinary variation.
    pub fn is_plausible(&self) -> bool {
        self.g_timing < 30.0
    }

    /// Whether [`Self::skill`] is a measurement rather than a floor.
    ///
    /// Below [`SKILL_IDENTIFIABLE_MIN`] the conditional band shares have converged
    /// on fixed window-width ratios and the fit can no longer separate scores, so
    /// the value should be read as "at most this skilled". This only covers the
    /// bottom end; the corresponding ceiling needs the map's difficulty to check
    /// against [`SKILL_SATURATION_RATIO`], which [`FitQuality`] does not carry.
    ///
    /// A caller feeding this into pp does not strictly need to branch on it, since
    /// nothing this low earns pp at all — sunny awards none at or below 80%
    /// accuracy. It matters for calibration, where mistaking a floor for a
    /// measurement would corrupt a fit.
    pub fn is_identifiable(&self) -> bool {
        self.skill > SKILL_IDENTIFIABLE_MIN
    }
}

/// Fit a score and report how well the result actually explains it.
///
/// Always prefer this over [`skill_for_counts`] when the number feeds pp. A skill
/// estimate on its own gives no indication that the score was nothing like what
/// the model expects, and such scores do occur — see the module docs.
pub fn fit_with_quality(
    counts: &[u32; 6],
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
) -> FitQuality {
    let skill = skill_for_counts(counts, units, windows, model);
    let expected = expected_counts(units, windows, model, skill);
    let expected_total = expected.total();
    let observed_total: f64 = counts.iter().map(|&count| f64::from(count)).sum();

    if expected_total <= 0.0 || observed_total <= 0.0 {
        return FitQuality {
            skill,
            g_statistic: f64::INFINITY,
            g_timing: f64::INFINITY,
            excess_misses: 0.0,
            g_per_judgement: f64::INFINITY,
        };
    }

    let miss = ManiaJudgement::Miss as usize;
    let observed_misses = f64::from(counts[miss]);
    let predicted_misses = expected.get(ManiaJudgement::Miss) / expected_total * observed_total;

    // Totals restricted to the timing judgements, used for the offset-sensitive
    // statistic. Conditioning on "the note was hit at all" is what makes dropped
    // notes drop out of the comparison instead of swamping it.
    let observed_timing = observed_total - observed_misses;
    let expected_timing = expected_total - expected.get(ManiaJudgement::Miss);

    let mut g_statistic = 0.0;
    let mut g_timing = 0.0;

    for judgement in ManiaJudgement::ALL {
        let observed = f64::from(counts[judgement as usize]);

        if observed <= 0.0 {
            continue;
        }

        // Rescale to the observed total so a passed-objects mismatch between the
        // score and the unit list does not read as a bad fit on its own.
        let predicted = (expected.get(judgement) / expected_total * observed_total).max(1e-12);

        g_statistic += 2.0 * observed * (observed / predicted).ln();

        if judgement != ManiaJudgement::Miss && observed_timing > 0.0 && expected_timing > 0.0 {
            let share = expected.get(judgement) / expected_timing;
            let predicted_timing = (share * observed_timing).max(1e-12);

            g_timing += 2.0 * observed * (observed / predicted_timing).ln();
        }
    }

    FitQuality {
        skill,
        g_statistic,
        g_timing,
        excess_misses: (observed_misses - predicted_misses).max(0.0),
        g_per_judgement: g_statistic / observed_total,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A representative OD9 non-convert window set, computed by hand from
    /// `34 + 3 * (10 - 9)` and friends so the tests do not depend on the window
    /// module's construction path.
    fn od9_windows() -> ManiaHitWindows {
        ManiaHitWindows {
            perfect: 16.5,
            great: 37.5,
            good: 70.5,
            ok: 100.5,
            meh: 124.5,
            miss: 161.5,
        }
    }

    /// The same map under `EZ`, i.e. every window multiplied by 1.4.
    fn od9_ez_windows() -> ManiaHitWindows {
        ManiaHitWindows {
            perfect: 22.5,
            great: 52.5,
            good: 98.5,
            ok: 140.5,
            meh: 174.5,
            miss: 226.5,
        }
    }

    fn uniform_units(difficulty: f64, count: usize) -> Vec<JudgementUnit> {
        vec![JudgementUnit::new(difficulty); count]
    }

    /// The whole reason the two-unit "share" form is legitimate: `expected_counts` is
    /// linear in the weights, so collapsing identical units into one weighted entry is
    /// exact, not an approximation.
    ///
    /// Worth pinning because the collapsed form looks like it is averaging when it is
    /// in fact summing. If this ever fails, every LN-share figure becomes suspect.
    #[test]
    fn collapsing_identical_units_is_exact_not_an_approximation() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let scale = ln_sigma_scale(1.6);

        // 300 rice notes and 700 long notes, spelled out one judgement at a time.
        let mut spelled = vec![JudgementUnit::new(6.0); 300];
        spelled.extend(vec![JudgementUnit::new(6.0).with_sigma_scale(scale); 700]);

        let collapsed = [
            JudgementUnit::repeated(6.0, 300.0),
            JudgementUnit::repeated(6.0, 700.0).with_sigma_scale(scale),
        ];

        let from_spelled = expected_counts(&spelled, &windows, &model, 7.0).as_array();
        let from_collapsed = expected_counts(&collapsed, &windows, &model, 7.0).as_array();

        for (a, b) in from_spelled.iter().zip(from_collapsed.iter()) {
            assert!(
                (a - b).abs() < 1e-9,
                "collapsed weights must reproduce the per-note sum exactly: {a} vs {b}"
            );
        }
    }

    /// A mixture of two widths must be strictly harder than the narrow population
    /// alone and strictly easier than the wide one alone.
    ///
    /// This is the sanity check on the LN split's direction: adding long notes to a map
    /// can only cost PERFECTs at fixed skill, never gain them.
    #[test]
    fn an_ln_mixture_sits_between_its_two_populations() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let scale = ln_sigma_scale(1.0);

        let perfect_share = |units: &[JudgementUnit]| {
            let counts = expected_counts(units, &windows, &model, 7.0);
            counts.get(ManiaJudgement::Perfect) / counts.total()
        };

        let all_rice = [JudgementUnit::repeated(6.0, 1000.0)];
        let half = [
            JudgementUnit::repeated(6.0, 500.0),
            JudgementUnit::repeated(6.0, 500.0).with_sigma_scale(scale),
        ];
        let all_ln = [JudgementUnit::repeated(6.0, 1000.0).with_sigma_scale(scale)];

        let (rice, mixed, ln) = (
            perfect_share(&all_rice),
            perfect_share(&half),
            perfect_share(&all_ln),
        );

        assert!(
            rice > mixed && mixed > ln,
            "a mixture must sit strictly between its components: {rice} / {mixed} / {ln}"
        );
    }

    /// `sqrt(1 + k^2)` at `k = 1` is `sqrt(2)`, and a release cannot be easier to place
    /// than a press.
    #[test]
    fn the_ln_scale_reduces_to_sqrt_two_without_asymmetry() {
        assert!((ln_sigma_scale(1.0) - LN_SIGMA_SCALE).abs() < 1e-12);

        // Clamped, not extrapolated: a sub-1 ratio would claim releases are more
        // precise than presses, which is not a regime we model.
        assert!((ln_sigma_scale(0.5) - LN_SIGMA_SCALE).abs() < 1e-12);
        assert!((ln_sigma_scale(f64::NAN) - LN_SIGMA_SCALE).abs() < 1e-12);

        // Monotone above 1, and matching the closed form.
        assert!(ln_sigma_scale(2.0) > ln_sigma_scale(1.5));
        assert!((ln_sigma_scale(2.0) - 5.0_f64.sqrt()).abs() < 1e-12);
    }

    /// A degenerate `sigma_scale` must not turn a hard unit into a free one.
    ///
    /// The scale reaches sigma multiplicatively, so a zero or negative value would
    /// collapse the distribution and make every judgement a certain PERFECT — a
    /// silently unbounded score. It is treated as "no scaling" instead.
    #[test]
    fn a_degenerate_sigma_scale_cannot_manufacture_perfects() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        let baseline = judgement_probabilities(&windows, &model, 6.0, 7.0);

        for bad in [0.0, -1.0, f64::NAN, f64::INFINITY] {
            let probabilities =
                judgement_probabilities_scaled(&windows, &model, 6.0, 7.0, bad, 0.0);

            assert_eq!(
                probabilities.get(ManiaJudgement::Perfect),
                baseline.get(ManiaJudgement::Perfect),
                "a {bad} scale must fall back to no scaling, not a free PERFECT"
            );
        }
    }

    /// `release_mean_offset: 0.0` must reproduce, bit-for-bit, whatever
    /// [`expected_counts`] computed before the offset existed.
    ///
    /// This is the regression guard for [`ErrorModel::exceedance_with_offset`]: the
    /// zero-offset branch short-circuits to [`ErrorModel::exceedance`] rather than
    /// going through the two-`one_sided_tail`-term algebra, specifically so that a
    /// mixture with a hundredth-of-an-ulp rounding difference from the sum-of-two-halves
    /// path can never appear when the offset is off. The right-hand side here is
    /// built by hand from the *pre-offset* nested-tail loop — differencing
    /// `model.exceedance(upper, sigma)` directly, with no `mu` term anywhere — so
    /// this test would fail if that short-circuit were ever removed, even though the
    /// algebra it replaces is mathematically equal at `mu = 0`.
    #[test]
    fn zero_release_offset_is_bit_identical_to_the_old_path() {
        let windows = od9_windows();
        let model = ErrorModel {
            release_mean_offset: 0.0,
            ..ErrorModel::default()
        };
        let scale = ln_sigma_scale_for_duration(&model, 150.0);

        let units = [
            JudgementUnit::repeated(6.0, 300.0),
            JudgementUnit::long_note(6.0, 700.0, &model, 150.0),
        ];

        let skill = 6.3;
        let actual = expected_counts(&units, &windows, &model, skill).as_array();

        // The old algorithm, spelled out independently: no `mu`, no
        // `exceedance_with_offset`, just the original nested-tail loop over
        // `model.exceedance`.
        let old_probabilities = |difficulty: f64, sigma_scale: f64| -> [f64; 6] {
            let sigma = model.sigma(difficulty, skill) * sigma_scale;
            let mut probabilities = [0.0; 6];
            let mut remaining = 1.0;

            for judgement in ManiaJudgement::ALL {
                let (_, upper) = windows.band(judgement);
                let outside = model.exceedance(upper, sigma).min(remaining);
                probabilities[judgement as usize] = remaining - outside;
                remaining = outside;
            }

            probabilities
        };

        let mut expected = [0.0; 6];

        for (difficulty, weight, unit_scale) in [(6.0, 300.0, 1.0), (6.0, 700.0, scale)] {
            let probabilities = old_probabilities(difficulty, unit_scale);

            for judgement in ManiaJudgement::ALL {
                expected[judgement as usize] += weight * probabilities[judgement as usize];
            }
        }

        assert_eq!(
            actual, expected,
            "mu = 0.0 must be bit-for-bit identical to the pre-offset code path"
        );
    }

    #[test]
    fn recovery_offset_uses_the_fitted_curve_by_default() {
        let model = ErrorModel::default();

        assert_eq!(model.recovery_offset, 20.425);
        assert_eq!(model.recovery_tau, 116.68);
        assert_eq!(model.anticipation_offset, -2.517);
        assert!(model.recovery_mean_offset(50.0) > 0.0);
        assert!((model.recovery_mean_offset(850.0) - model.anticipation_offset).abs() < 0.02);
    }

    #[test]
    fn recovery_offset_follows_the_fitted_gap_curve() {
        let model = ErrorModel {
            recovery_offset: 20.425,
            recovery_tau: 116.68,
            anticipation_offset: -2.517,
            ..ErrorModel::default()
        };

        let at_zero = model.recovery_mean_offset(0.0);
        let at_tau = model.recovery_mean_offset(model.recovery_tau);
        let long_gap = model.recovery_mean_offset(850.0);

        assert!((at_zero - 17.908).abs() < 1e-10);
        assert!((at_tau - (20.425_f64 / std::f64::consts::E - 2.517)).abs() < 1e-10);
        assert!((long_gap + 2.517).abs() < 0.02);
        assert!(at_zero > at_tau);
        assert!(at_tau > long_gap);
    }

    #[test]
    fn recovery_offset_handles_missing_or_invalid_predecessors() {
        let model = ErrorModel {
            recovery_offset: 20.0,
            anticipation_offset: -3.19,
            ..ErrorModel::default()
        };

        assert_eq!(model.recovery_mean_offset(f64::INFINITY), 0.0);
        assert_eq!(model.recovery_mean_offset(f64::NAN), 0.0);
        assert_eq!(model.recovery_mean_offset(-1.0), 16.81);
    }

    #[test]
    fn erfc_matches_known_values() {
        for &(x, expected) in &[
            (0.0, 1.0),
            (0.5, 0.479_500_122),
            (1.0, 0.157_299_207),
            (2.0, 0.004_677_735),
            (-1.0, 1.842_700_792),
        ] {
            assert!(
                (erfc(x) - expected).abs() < 1e-6,
                "erfc({x}) = {}, expected {expected}",
                erfc(x)
            );
        }
    }

    #[test]
    fn erfc_stays_relatively_accurate_in_the_tail() {
        // The whole reason for using the Numerical Recipes form. An absolute-error
        // approximation returns exactly 0 here, which would flatten the surface
        // for every skilled player.
        for &(x, expected) in &[
            (4.0, 1.541_725_79e-8),
            (6.0, 2.151_973_6e-17),
            (10.0, 2.088_487_58e-45),
        ] {
            let actual = erfc(x);
            let relative_error = (actual - expected).abs() / expected;

            assert!(
                relative_error < 1e-6,
                "erfc({x}) = {actual}, expected {expected} (relative error {relative_error})"
            );
        }
    }

    #[test]
    fn probabilities_sum_to_one() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        for &difficulty in &[0.0, 1.0, 3.0, 6.0, 10.0, 25.0] {
            for &skill in &[0.5, 2.0, 5.0, 8.0, 20.0] {
                let probabilities = judgement_probabilities(&windows, &model, difficulty, skill);
                let sum: f64 = probabilities.as_array().iter().sum();

                assert!(
                    (sum - 1.0).abs() < 1e-9,
                    "difficulty {difficulty}, skill {skill}: sum {sum}"
                );
            }
        }
    }

    #[test]
    fn counts_always_sum_to_unit_total() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 500);

        for &skill in &[0.1, 1.0, 4.0, 5.0, 12.0, 100.0] {
            let counts = expected_counts(&units, &windows, &model, skill);

            assert!(
                (counts.total() - 500.0).abs() < 1e-6,
                "skill {skill}: total {}",
                counts.total()
            );
        }
    }

    #[test]
    fn accuracy_increases_with_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 200);

        let mut previous = -1.0;

        // Stops below the timing-saturation point on purpose; see
        // `timing_precision_saturates_but_the_slip_channel_remains`.
        for &skill in &[0.5, 1.0, 2.0, 4.0, 6.0, 10.0, 15.0] {
            let accuracy = expected_counts(&units, &windows, &model, skill).custom_accuracy();

            assert!(
                accuracy > previous,
                "skill {skill} gave {accuracy}, not above {previous}"
            );

            previous = accuracy;
        }
    }

    /// The floor is off by default and this is why: a real 1506-note score was judged
    /// entirely 320, and any meaningful floor makes that impossible regardless of
    /// skill. Guards the replay-measured 10 ms from being reintroduced as a default.
    #[test]
    fn a_sigma_floor_would_forbid_scores_that_exist() {
        let windows = od9_windows();

        assert_eq!(
            ErrorModel::default().sigma_floor,
            0.0,
            "the floor must stay off until the replay/counts conflict is resolved"
        );

        // Skill far past anything a player reaches, so only the floor can limit the
        // predicted 320 share.
        let skill = 1.0e4;
        let units = uniform_units(2.0, 1506);

        let floored = ErrorModel {
            sigma_floor: 10.0,
            ..Default::default()
        };
        let share = expected_counts(&units, &windows, &floored, skill).get(ManiaJudgement::Perfect)
            / 1506.0;

        assert!(
            share < 0.9,
            "a 10ms floor should cap the 320 share well under an SS, got {share}"
        );

        // Without it the same score is reachable, which is what the counts require.
        let share = expected_counts(&units, &windows, &ErrorModel::default(), skill)
            .get(ManiaJudgement::Perfect)
            / 1506.0;

        assert!(
            share > 1.0 - 1e-9,
            "with no floor an all-320 score must be reachable, got {share}"
        );
    }

    /// Where the counts actually place the ceiling, as opposed to merely rejecting
    /// 10 ms.
    ///
    /// The physical argument for a small floor is sound and independent of the
    /// replays: osu! judges at 1000 ticks per second, so 1 ms is a hard limit on the
    /// timing anyone can resolve, and keyboard scan plus OS scheduling jitter add a
    /// few ms above it. That reasoning suggests 1-5 ms. This pins how much of that
    /// band the all-320 score leaves open: 1 ms costs 0.01 of 1506 notes and is
    /// invisible, while 5 ms costs 25 notes and is refuted outright. The boundary
    /// sits near 2 ms.
    ///
    /// Kept separate from [`a_sigma_floor_would_forbid_scores_that_exist`] because
    /// that test guards the default; this one records the bound, so a future floor
    /// has a number to respect rather than having to rediscover it.
    #[test]
    #[ignore = "scratch"]
    fn print_floor_curve() {
        let windows = od9_windows();
        let units = uniform_units(2.0, 1506);
        for &floor in &[0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 8.0] {
            let model = ErrorModel {
                sigma_floor: floor,
                ..Default::default()
            };
            let share = expected_counts(&units, &windows, &model, 1.0e4)
                .get(ManiaJudgement::Perfect)
                / 1506.0;
            println!(
                "floor {floor:>4.1} ms -> {:>9.3} notes off 320",
                1506.0 * (1.0 - share)
            );
        }
    }

    #[test]
    fn the_counts_bound_a_floor_near_two_milliseconds() {
        let windows = od9_windows();
        let skill = 1.0e4;
        let units = uniform_units(2.0, 1506);

        let forced_off_320 = |floor: f64| {
            let model = ErrorModel {
                sigma_floor: floor,
                ..Default::default()
            };
            let share = expected_counts(&units, &windows, &model, skill)
                .get(ManiaJudgement::Perfect)
                / 1506.0;
            1506.0 * (1.0 - share)
        };

        // A 1 ms floor is consistent with the score: it displaces a hundredth of a
        // note, so the observation cannot argue against the physical limit itself.
        assert!(
            forced_off_320(1.0) < 0.1,
            "1ms should be invisible to a 1506-note SS, got {} notes",
            forced_off_320(1.0)
        );

        // By 5 ms it is not survivable — over one percent of the score would have to
        // have missed the window, and none did. The bound was ~25 notes under the old
        // 0.034/4.4 parameters; the narrower lapse tail (0.0296/3.339) lowers it to
        // ~16 because sigma_floor elevates less mass from a thinner tail.
        assert!(
            forced_off_320(5.0) > 15.0,
            "5ms should be clearly refuted, got {} notes",
            forced_off_320(5.0)
        );

        // Monotone in between, so "the bound" is a single crossing rather than a
        // region. Under the new 0.0296/3.339 parameters, 2ms costs ~0.6 notes (starting
        // to matter) and the 1-note crossing sits around 2.3-2.4ms.
        assert!(forced_off_320(2.0) > forced_off_320(1.0));
        assert!(forced_off_320(3.0) > forced_off_320(2.0));
        assert!(
            forced_off_320(2.0) > 0.5 && forced_off_320(3.0) > 1.0,
            "boundary should be between 2-3ms, got 2ms={} 3ms={}",
            forced_off_320(2.0),
            forced_off_320(3.0)
        );
    }

    #[test]
    fn timing_precision_saturates_into_a_clean_ss() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 200;
        let units = uniform_units(5.0, notes);

        // Past ~20 skill units on an OD9 map, sigma is small enough that the chance
        // of missing the 16.5ms PERFECT window underflows f64, so timing stops
        // distinguishing scores. With no lapse channel there is nothing left behind
        // it: the limit is a clean SS rather than `n * (1 - slip_rate)`.
        let below = expected_counts(&units, &windows, &model, 18.0);
        let above = expected_counts(&units, &windows, &model, 40.0);

        assert!(above.get(ManiaJudgement::Perfect) > below.get(ManiaJudgement::Perfect));

        assert!(
            (above.get(ManiaJudgement::Perfect) - notes as f64).abs() < 1e-6,
            "expected all {notes} notes PERFECT, got {}",
            above.get(ManiaJudgement::Perfect)
        );
        assert!(
            above.get(ManiaJudgement::Miss) < 1e-9,
            "a saturated score should predict no misses, got {}",
            above.get(ManiaJudgement::Miss)
        );
    }

    #[test]
    fn an_all_perfect_score_is_an_ordinary_prediction() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 1300;
        let units = uniform_units(5.0, notes);

        // An SS is the most common shape of a top score, so it has to be scoreable
        // and it should not be exotic. Under the old flat lapse channel it cost a
        // factor of `0.995^1300` ≈ 0.15%; with misses coming from the timing tail it
        // is simply what a player above the map is expected to do.
        let all_perfect = [notes as u32, 0, 0, 0, 0, 0];

        let skill = skill_for_counts(&all_perfect, &units, &windows, &model);
        let likelihood = log_likelihood(&all_perfect, &units, &windows, &model, skill);

        assert!(
            likelihood.is_finite(),
            "an SS must have finite likelihood, got {likelihood}"
        );

        // And it should be explained by high skill, not by a mediocre fit.
        assert!(skill > 10.0, "an SS should imply high skill, got {skill}");

        let quality = fit_with_quality(&all_perfect, &units, &windows, &model);

        assert!(
            quality.is_plausible(),
            "an SS should not be flagged as implausible, G was {}",
            quality.g_statistic
        );
    }

    #[test]
    fn misses_saturate_at_low_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(6.0, 100);

        // Far below the map's difficulty, essentially everything is dropped —
        // the flat left end of the miss curve in the design sketches.
        let counts = expected_counts(&units, &windows, &model, 0.3);

        assert!(
            counts.get(ManiaJudgement::Miss) > 95.0,
            "expected near-total misses, got {}",
            counts.get(ManiaJudgement::Miss)
        );
    }

    /// The skill at which a rising judgement probability first reaches `target`.
    ///
    /// Panics if the target is not bracketed, so a test cannot silently assert
    /// against a bracket edge.
    fn skill_where_rising(
        windows: &ManiaHitWindows,
        model: &ErrorModel,
        difficulty: f64,
        judgement: ManiaJudgement,
        target: f64,
    ) -> f64 {
        let probability =
            |skill: f64| judgement_probabilities(windows, model, difficulty, skill).get(judgement);

        assert!(
            probability(SKILL_MIN) < target && probability(SKILL_MAX) >= target,
            "{judgement:?} target {target} is not bracketed"
        );

        let mut low = SKILL_MIN;
        let mut high = SKILL_MAX;

        for _ in 0..200 {
            let mid = (low * high).sqrt();

            if probability(mid) < target {
                low = mid;
            } else {
                high = mid;
            }
        }

        (low * high).sqrt()
    }

    /// The skill at which a falling judgement probability drops to `target`.
    ///
    /// Separate from [`skill_where_rising`] because P(miss) decreases with skill,
    /// and reusing the rising search on it silently returns the bracket edge.
    fn skill_where_falling(
        windows: &ManiaHitWindows,
        model: &ErrorModel,
        difficulty: f64,
        judgement: ManiaJudgement,
        target: f64,
    ) -> f64 {
        let probability =
            |skill: f64| judgement_probabilities(windows, model, difficulty, skill).get(judgement);

        assert!(
            probability(SKILL_MIN) > target && probability(SKILL_MAX) <= target,
            "{judgement:?} target {target} is not bracketed"
        );

        let mut low = SKILL_MIN;
        let mut high = SKILL_MAX;

        for _ in 0..200 {
            let mid = (low * high).sqrt();

            if probability(mid) > target {
                low = mid;
            } else {
                high = mid;
            }
        }

        (low * high).sqrt()
    }

    #[test]
    fn misses_resolve_before_the_perfect_curve_takes_off() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let difficulty = 6.0;

        // The design's shape claim: the miss curve is done early, and the PERFECT
        // curve only climbs afterwards, so the two occupy different regions of the
        // skill axis. Stated as an ordering rather than as counts at hand-picked
        // skill levels, since the ordering is what the surface actually needs.
        let misses_resolved =
            skill_where_falling(&windows, &model, difficulty, ManiaJudgement::Miss, 0.1);
        let perfect_takes_off =
            skill_where_rising(&windows, &model, difficulty, ManiaJudgement::Perfect, 0.5);

        assert!(
            perfect_takes_off > misses_resolved,
            "PERFECT should climb after misses resolve: {perfect_takes_off} vs {misses_resolved}"
        );
    }

    #[test]
    fn perfect_curve_rises_monotonically_and_steeply() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let difficulty = 6.0;

        let low = skill_where_rising(&windows, &model, difficulty, ManiaJudgement::Perfect, 0.10);
        let high = skill_where_rising(&windows, &model, difficulty, ManiaJudgement::Perfect, 0.90);

        // 10% to 90% inside a factor of ~5 in skill. This is the knee the whole
        // design hinges on: it is what makes a widened 320 window translate into a
        // materially different implied skill.
        assert!(high > low);
        assert!(
            high / low < 6.0,
            "PERFECT knee too gradual: {low} to {high} is a factor of {}",
            high / low
        );

        let units = uniform_units(difficulty, 100);
        let mut previous = -1.0;

        for &skill in &[1.0, 2.0, 4.0, 6.0, 8.0, 12.0] {
            let perfect =
                expected_counts(&units, &windows, &model, skill).get(ManiaJudgement::Perfect);

            assert!(
                perfect > previous,
                "skill {skill}: {perfect} not above {previous}"
            );

            previous = perfect;
        }
    }

    #[test]
    fn perfect_window_sets_the_knee_position() {
        let model = ErrorModel::default();
        let difficulty = 6.0;

        // The claim that makes this replace the EZ multiplier: the knee moves
        // because the PERFECT window moved, nothing else.
        let plain = skill_where_rising(
            &od9_windows(),
            &model,
            difficulty,
            ManiaJudgement::Perfect,
            0.5,
        );
        let with_ez = skill_where_rising(
            &od9_ez_windows(),
            &model,
            difficulty,
            ManiaJudgement::Perfect,
            0.5,
        );

        assert!(
            with_ez < plain,
            "a wider PERFECT window should shift the knee left: {with_ez} vs {plain}"
        );
    }

    #[test]
    fn skill_is_never_unbounded_for_reachable_accuracy() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 300);

        let skill = skill_for_accuracy(&units, &windows, &model, 0.97);

        assert!(
            skill > SKILL_MIN && skill < SKILL_MAX,
            "skill {skill} hit a bracket edge"
        );
    }

    #[test]
    fn accuracy_inversion_round_trips() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.5, 400);

        for &target in &[0.60, 0.80, 0.93, 0.97, 0.99] {
            let skill = skill_for_accuracy(&units, &windows, &model, target);
            let achieved = expected_counts(&units, &windows, &model, skill).custom_accuracy();

            assert!(
                (achieved - target).abs() < 1e-6,
                "target {target} recovered as {achieved} at skill {skill}"
            );
        }
    }

    #[test]
    fn ez_requires_more_skill_for_the_same_accuracy() {
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 500);

        // The crux of the redesign. The same judgement counts on the same
        // pattern imply *less* skill once EZ widens the windows, so no
        // EZ-specific pp multiplier is needed to undo the advantage.
        let plain = skill_for_accuracy(&units, &od9_windows(), &model, 0.98);
        let with_ez = skill_for_accuracy(&units, &od9_ez_windows(), &model, 0.98);

        assert!(
            with_ez < plain,
            "EZ should lower the implied skill: {with_ez} vs {plain}"
        );

        // And the gap is substantial, not a rounding artifact like the
        // hit_leniency clamp currently produces.
        let ratio = with_ez / plain;

        assert!(
            ratio < 0.90,
            "EZ discount should be material, ratio was {ratio}"
        );
    }

    #[test]
    fn ez_advantage_shows_up_in_perfect_counts() {
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 500);

        // At identical skill, EZ's wider 320 window yields more 320s. The
        // current pipeline cannot see this at all, since it models no PERFECT
        // window.
        let plain = expected_counts(&units, &od9_windows(), &model, 6.0);
        let with_ez = expected_counts(&units, &od9_ez_windows(), &model, 6.0);

        assert!(
            with_ez.get(ManiaJudgement::Perfect) > plain.get(ManiaJudgement::Perfect) * 1.2,
            "EZ PERFECT {} vs plain {}",
            with_ez.get(ManiaJudgement::Perfect),
            plain.get(ManiaJudgement::Perfect)
        );
    }

    #[test]
    fn likelihood_fit_recovers_the_generating_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        for &truth in &[3.0, 5.0, 8.0] {
            let counts = expected_counts(&units, &windows, &model, truth).round_to_hits(1000);
            let recovered = skill_for_counts(&counts, &units, &windows, &model);

            // Rounding to whole notes limits how exactly this can come back.
            assert!(
                (recovered - truth).abs() / truth < 0.05,
                "truth {truth} recovered as {recovered}"
            );
        }
    }

    #[test]
    fn rounding_preserves_the_total() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(4.0, 733);

        for &skill in &[1.0, 4.0, 9.0] {
            let counts = expected_counts(&units, &windows, &model, skill).round_to_hits(733);
            let total: u32 = counts.iter().sum();

            assert_eq!(total, 733, "skill {skill} rounded to {total}");
        }
    }

    #[test]
    fn harder_patterns_need_more_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        let easy = uniform_units(3.0, 300);
        let hard = uniform_units(7.0, 300);

        let easy_skill = skill_for_accuracy(&easy, &windows, &model, 0.97);
        let hard_skill = skill_for_accuracy(&hard, &windows, &model, 0.97);

        assert!(
            hard_skill > easy_skill,
            "hard {hard_skill} should exceed easy {easy_skill}"
        );
    }

    #[test]
    fn weighted_units_match_repeated_ones() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        let expanded = uniform_units(5.0, 40);
        let collapsed = vec![JudgementUnit::repeated(5.0, 40.0)];

        let a = expected_counts(&expanded, &windows, &model, 4.0);
        let b = expected_counts(&collapsed, &windows, &model, 4.0);

        for judgement in ManiaJudgement::ALL {
            assert!(
                (a.get(judgement) - b.get(judgement)).abs() < 1e-9,
                "{judgement:?}: {} vs {}",
                a.get(judgement),
                b.get(judgement)
            );
        }
    }

    #[test]
    fn zero_skill_is_all_misses() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 50);

        let counts = expected_counts(&units, &windows, &model, 0.0);

        assert!((counts.get(ManiaJudgement::Miss) - 50.0).abs() < 1e-9);
        assert!((counts.custom_accuracy() - 0.0).abs() < 1e-9);
    }

    /// A judgement vector generated by the model itself at the given skill, so a
    /// test can perturb a realistic score rather than a hand-written one.
    fn synthetic_score(
        units: &[JudgementUnit],
        windows: &ManiaHitWindows,
        model: &ErrorModel,
        skill: f64,
        total: u32,
    ) -> [u32; 6] {
        expected_counts(units, windows, model, skill).round_to_hits(total)
    }

    #[test]
    fn a_few_stray_misses_barely_move_the_estimate() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        let clean = synthetic_score(&units, &windows, &model, 5.0, 1000);
        let baseline = skill_for_counts(&clean, &units, &windows, &model);

        // Slipped fingers and dropped inputs are not timing errors. Without a slip
        // rate these cost up to 28% of estimated skill; with one they should be
        // nearly free.
        for &extra in &[1, 5, 20] {
            let mut perturbed = clean;
            let moved = extra.min(perturbed[ManiaJudgement::Perfect as usize]);
            perturbed[ManiaJudgement::Perfect as usize] -= moved;
            perturbed[ManiaJudgement::Miss as usize] += moved;

            let estimate = skill_for_counts(&perturbed, &units, &windows, &model);
            let shift = (estimate - baseline).abs() / baseline;

            assert!(
                shift < 0.05,
                "{extra} stray misses moved skill by {:.1}% ({baseline} -> {estimate})",
                shift * 100.0
            );
        }
    }

    /// Supersedes an earlier test that asserted `slip_rate` was the mechanism: the fit
    /// conditions misses out of the likelihood entirely, so `slip_rate` cannot move
    /// it. This is what makes the estimate independent of a constant nobody has
    /// calibrated yet.
    #[test]
    fn the_fit_does_not_depend_on_the_slip_rate() {
        let windows = od9_windows();
        let units = uniform_units(5.0, 1000);

        let mut score = synthetic_score(&units, &windows, &ErrorModel::default(), 5.0, 1000);
        score[ManiaJudgement::Perfect as usize] -= 20;
        score[ManiaJudgement::Miss as usize] += 20;

        let baseline = skill_for_counts(
            &score,
            &units,
            &windows,
            &ErrorModel {
                slip_rate: 0.0,
                ..ErrorModel::default()
            },
        );

        // Slip rates spanning four orders of magnitude, including absurd ones, must
        // all leave the estimate untouched. The cancellation is exact in principle —
        // slip scales every timing band by the same `1 - slip` factor, which divides
        // out when the bands are renormalised among themselves — so the tolerance
        // here only has to absorb golden-section rounding, not any real dependence.
        for &slip in &[0.0, 0.001, 0.005, 0.05, 0.5] {
            let model = ErrorModel {
                slip_rate: slip,
                ..ErrorModel::default()
            };
            let estimate = skill_for_counts(&score, &units, &windows, &model);

            assert!(
                (estimate - baseline).abs() / baseline < 1e-6,
                "slip_rate {slip} changed the fit: {estimate} vs {baseline}"
            );
        }
    }

    /// Why conditioning is the right call: misses carry no timing information for
    /// anyone competent, but they do for a weak player.
    #[test]
    fn the_timing_model_alone_predicts_no_misses_for_a_good_score() {
        let windows = od9_windows();
        let timing_only = ErrorModel {
            slip_rate: 0.0,
            ..ErrorModel::default()
        };

        // A weak player misses because their error spread genuinely reaches past the
        // MEH window. Those misses are real timing evidence, and the surface produces
        // them without any lapse term.
        let units = uniform_units(5.0, 1300);
        let weak = expected_counts(&units, &windows, &timing_only, 1.5);

        assert!(
            weak.get(ManiaJudgement::Miss) > 100.0,
            "a weak player should miss from timing alone, got {}",
            weak.get(ManiaJudgement::Miss)
        );

        // A player comfortably above the difficulty does not, at any map length — so
        // every miss in such a score comes from outside the timing model.
        //
        // The threshold is a skill *ratio* rather than an absolute skill, and the
        // margin is wider than it was under a single normal. The calibrated mixture
        // has a lapse component 4.4x the core width, so it keeps predicting a small
        // number of misses through the region where a lone normal had already cut off:
        // at difficulty 5 on 1000 notes it gives ~6.6 misses at ratio 1.0 and ~2.6 at
        // 1.2, reaching 0.13 only by ratio 1.6. That is the shape the real scores
        // demanded — see the module docs — and it is more honest besides, since a
        // player at their limit does drop notes. The claim that survives is the one
        // conditioning actually rests on: once a player is clear of the difficulty,
        // timing stops explaining misses.
        for &notes in &[500usize, 1300, 5000] {
            let units = uniform_units(5.0, notes);

            for &skill in &[10.0, 15.0, 20.0] {
                let counts = expected_counts(&units, &windows, &timing_only, skill);

                assert!(
                    counts.get(ManiaJudgement::Miss) < 1.0,
                    "skill {skill} on {notes} notes predicted {} misses",
                    counts.get(ManiaJudgement::Miss)
                );
            }
        }
    }

    /// The mixture's tail is what lets one skill value produce a sharp 320 bulk and a
    /// populated 100/50 tail at once. A single normal cannot, and that failure is
    /// exactly what the 20 real scores showed.
    ///
    /// Pins the property rather than the constants: whatever the calibration settles
    /// on, turning the lapse component off must make the far tail thinner while the
    /// core gets no sharper.
    #[test]
    fn the_lapse_component_thickens_the_tail_without_blunting_the_core() {
        let windows = od9_windows();
        let units = uniform_units(5.0, 2000);

        let mixture = ErrorModel::default();
        let single = ErrorModel {
            lapse_weight: 0.0,
            ..mixture
        };

        // Compared at equal sigma, i.e. the same core width, so the difference is the
        // tail alone rather than a rescaling.
        let skill = 7.5;
        let with_lapse = expected_counts(&units, &windows, &mixture, skill);
        let without = expected_counts(&units, &windows, &single, skill);

        assert!(
            with_lapse.get(ManiaJudgement::Meh) > 4.0 * without.get(ManiaJudgement::Meh),
            "the lapse component should populate the far tail: {} vs {}",
            with_lapse.get(ManiaJudgement::Meh),
            without.get(ManiaJudgement::Meh)
        );

        assert!(
            with_lapse.get(ManiaJudgement::Perfect) < without.get(ManiaJudgement::Perfect),
            "moving mass into the tail must come out of the core"
        );

        // The point of the exercise: at a *fitted* skill the mixture reproduces both
        // ends better than a normal can. Take counts the mixture itself generates,
        // round them to a real score, and confirm the mixture explains it better.
        let counts = with_lapse.round_to_hits(2000);

        let mixture_fit = fit_with_quality(&counts, &units, &windows, &mixture);
        let single_fit = fit_with_quality(&counts, &units, &windows, &single);

        assert!(
            mixture_fit.g_timing < single_fit.g_timing,
            "the generating model should fit its own output better: {} vs {}",
            mixture_fit.g_timing,
            single_fit.g_timing
        );
    }

    /// `sigma_ref` is a gauge parameter, not a calibratable one: it fixes the units
    /// skill is measured in and nothing observable depends on it, because skill is
    /// refit per score and absorbs it exactly.
    ///
    /// This is why the calibration in `sunny::tests::calibration_search` searches the
    /// two shape parameters only. Discovered by sweeping it over a 16x range and
    /// watching `g_timing` stay identical to four decimals.
    #[test]
    fn sigma_ref_only_sets_the_scale_of_skill() {
        let windows = od9_windows();
        let units = uniform_units(5.0, 1500);
        let counts = [700u32, 550, 200, 40, 8, 2];

        let base = ErrorModel::default();
        let baseline = fit_with_quality(&counts, &units, &windows, &base);

        for &factor in &[0.25, 0.5, 2.0, 4.0] {
            let scaled = ErrorModel {
                sigma_ref: base.sigma_ref * factor,
                ..base
            };
            let fit = fit_with_quality(&counts, &units, &windows, &scaled);

            assert!(
                (fit.g_timing - baseline.g_timing).abs() < 1e-6,
                "sigma_ref {factor}x changed the fit quality: {} vs {}",
                fit.g_timing,
                baseline.g_timing
            );

            // Skill absorbs it as `factor^(1/skill_exponent)`, which is what makes the
            // window scalar — a ratio of two skills — invariant to the choice.
            let predicted = baseline.skill * factor.powf(1.0 / base.skill_exponent);

            assert!(
                (fit.skill / predicted - 1.0).abs() < 1e-3,
                "skill should scale as factor^(1/skill_exponent): got {} expected {predicted}",
                fit.skill
            );
        }
    }

    #[test]
    fn within_play_skill_variation_biases_downward() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // Half the map played below the player's average and half above. The fit
        // cannot represent this, and the safe direction to be wrong is downward.
        let truth = 5.0;

        for &spread in &[0.2, 0.5] {
            let low = expected_counts(&units, &windows, &model, truth * (1.0 - spread));
            let high = expected_counts(&units, &windows, &model, truth * (1.0 + spread));

            let mut mixed = [0u32; 6];

            for judgement in ManiaJudgement::ALL {
                let combined = (low.get(judgement) + high.get(judgement)) / 2.0;
                mixed[judgement as usize] = combined.round() as u32;
            }

            let estimate = skill_for_counts(&mixed, &units, &windows, &model);

            assert!(
                estimate < truth,
                "spread {spread} gave {estimate}, which is not below {truth}"
            );
        }
    }

    #[test]
    fn per_note_difficulty_spread_biases_downward() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let truth = 5.0;

        // The deviation most likely to occur in practice: the real map has a range
        // of local difficulties but the estimate is fitted against their average.
        let varied: Vec<_> = (0..1000)
            .map(|index| {
                let difficulty = if index % 2 == 0 { 3.0 } else { 7.0 };

                JudgementUnit::new(difficulty)
            })
            .collect();
        let averaged = uniform_units(5.0, 1000);

        let counts = expected_counts(&varied, &windows, &model, truth).round_to_hits(1000);
        let estimate = skill_for_counts(&counts, &averaged, &windows, &model);

        assert!(
            estimate < truth,
            "difficulty spread should not inflate skill: {estimate} vs {truth}"
        );
    }

    #[test]
    fn a_well_behaved_score_is_reported_as_plausible() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        for &skill in &[3.0, 5.0, 8.0] {
            let counts = synthetic_score(&units, &windows, &model, skill, 1000);
            let quality = fit_with_quality(&counts, &units, &windows, &model);

            assert!(
                quality.is_plausible(),
                "skill {skill} scored G = {} and should be plausible",
                quality.g_statistic
            );
        }
    }

    #[test]
    fn a_score_no_single_skill_explains_is_flagged() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // The bimodal case: a chunk of the map trivially PERFECTed and the rest
        // dropped, which is roughly the shape of a vibro farm play. No single skill
        // level produces this, and reporting a number for it without comment would
        // be the dangerous outcome.
        let easy = expected_counts(&units, &windows, &model, 12.0);
        let hard = expected_counts(&units, &windows, &model, 1.5);

        let mut bimodal = [0u32; 6];

        for judgement in ManiaJudgement::ALL {
            let combined = easy.get(judgement) * 0.7 + hard.get(judgement) * 0.3;
            bimodal[judgement as usize] = combined.round() as u32;
        }

        let quality = fit_with_quality(&bimodal, &units, &windows, &model);

        assert!(
            !quality.is_plausible(),
            "a bimodal score should be flagged, G was {}",
            quality.g_statistic
        );
    }

    #[test]
    fn a_fixed_g_threshold_works_at_every_map_length() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        // `g_statistic` is the length-independent figure: it is a chi-squared-like
        // quantity with fixed degrees of freedom, so a well-behaved score sits in
        // the same range whether the map is short or long. That is what makes a
        // single `is_plausible` cutoff valid across map sizes.
        for &notes in &[300usize, 1000, 5000, 10000] {
            let units = uniform_units(5.0, notes);
            let counts = synthetic_score(&units, &windows, &model, 5.0, notes as u32);
            let quality = fit_with_quality(&counts, &units, &windows, &model);

            assert!(
                quality.is_plausible(),
                "a clean {notes}-note score should pass, G was {}",
                quality.g_statistic
            );
        }
    }

    #[test]
    fn per_judgement_g_falls_with_length_and_is_not_a_threshold() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        // Recorded because it is a trap: `g_per_judgement` divides an already
        // length-independent statistic by note count, so it shrinks as `1/n`. A
        // fixed cutoff on it would be drastically stricter on short maps. Kept as a
        // diagnostic only, and pinned here so nobody mistakes it for a threshold.
        let mut previous = f64::INFINITY;

        for &notes in &[500usize, 2000, 10000] {
            let units = uniform_units(5.0, notes);
            let counts = synthetic_score(&units, &windows, &model, 5.0, notes as u32);
            let quality = fit_with_quality(&counts, &units, &windows, &model);

            assert!(
                quality.g_per_judgement < previous,
                "per-judgement G should fall with length: {notes} notes gave {}",
                quality.g_per_judgement
            );

            previous = quality.g_per_judgement;
        }
    }

    #[test]
    fn score_totals_need_not_match_the_unit_count() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // A failed or partial play submits fewer judgements than the map has notes.
        // That is a length mismatch, not a badly-shaped score, so it must not read
        // as a bad fit.
        let full = synthetic_score(&units, &windows, &model, 5.0, 1000);
        let mut partial = [0u32; 6];

        for judgement in ManiaJudgement::ALL {
            partial[judgement as usize] = full[judgement as usize] / 2;
        }

        let quality = fit_with_quality(&partial, &units, &windows, &model);

        assert!(
            quality.is_plausible(),
            "a half-length score should still fit, G was {}",
            quality.g_statistic
        );
    }

    #[test]
    fn empty_and_degenerate_scores_do_not_panic() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 100);

        let all_misses = [0, 0, 0, 0, 0, 100];
        let quality = fit_with_quality(&all_misses, &units, &windows, &model);

        assert!(quality.skill.is_finite());
        assert!(quality.g_statistic >= 0.0);

        let empty = [0u32; 6];
        let quality = fit_with_quality(&empty, &units, &windows, &model);

        assert!(quality.skill.is_finite());

        let no_units = fit_with_quality(&all_misses, &[], &windows, &model);

        assert!(no_units.skill.is_finite());
    }

    #[test]
    fn nan_and_negative_skill_do_not_propagate() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        for &skill in &[f64::NAN, -1.0, -0.0] {
            let probabilities = judgement_probabilities(&windows, &model, 5.0, skill);
            let sum: f64 = probabilities.as_array().iter().sum();

            assert!(
                (sum - 1.0).abs() < 1e-12,
                "skill {skill} gave a distribution summing to {sum}"
            );
            assert!(
                (probabilities.get(ManiaJudgement::Miss) - 1.0).abs() < 1e-12,
                "skill {skill} should be all misses"
            );
        }
    }

    #[test]
    fn saturated_scores_report_the_lowest_explaining_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // A perfect score is explicable by any skill past saturation, so the fit
        // is only a lower bound. It must still land at the bottom of the plateau
        // rather than at the bracket edge, otherwise an SS would read as
        // arbitrarily skilled.
        let notes = 1000;
        let all_perfect = [notes as u32, 0, 0, 0, 0, 0];
        let skill = skill_for_counts(&all_perfect, &units, &windows, &model);

        assert!(
            skill < SKILL_MAX,
            "an SS should not report the bracket edge, got {skill}"
        );

        // Asserted in count form: at the reported skill essentially every note is
        // expected to be a PERFECT. With the slip channel at zero there is no
        // residue to allow for, so the shortfall should be numerically negligible.
        let counts = expected_counts(&units, &windows, &model, skill);
        let shortfall = notes as f64 - counts.get(ManiaJudgement::Perfect);

        assert!(
            shortfall < 1e-6,
            "the reported skill should leave nothing unaccounted for, shortfall {shortfall}"
        );
    }

    /// An SS must be the ordinary prediction for a player above the map, not an
    /// exponentially unlikely one. The flat lapse channel failed this on length: at
    /// `0.005` it put an SS on 6358 notes at `1.4e-14` and predicted 32 misses for a
    /// score that had none.
    #[test]
    fn a_clean_score_predicts_no_misses_at_any_length() {
        let windows = od9_windows();
        let model = ErrorModel::default();

        assert_eq!(
            model.slip_rate, 0.0,
            "misses should come from the timing tail"
        );

        for &notes in &[100, 1300, 6358] {
            let units = uniform_units(5.0, notes);
            let counts = expected_counts(&units, &windows, &model, 5000.0);

            assert!(
                counts.get(ManiaJudgement::Miss) < 1e-9,
                "{notes} notes predicted {} misses for a comfortable player",
                counts.get(ManiaJudgement::Miss)
            );

            assert_eq!(
                counts.custom_accuracy(),
                1.0,
                "an SS should be reachable in the mean at {notes} notes"
            );
        }
    }

    /// The property the flat channel could not have: a wider MISS window moves mass
    /// back out of MISS, so EZ predicts strictly fewer misses than NM at equal skill.
    #[test]
    fn a_wider_miss_window_predicts_fewer_misses() {
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // Skill low enough that the timing tail genuinely reaches past MEH, which is
        // the only regime where the miss count carries information.
        let skill = 2.0;

        let plain = od9_windows();
        let widened = od9_ez_windows();

        let plain_misses = expected_counts(&units, &plain, &model, skill).get(ManiaJudgement::Miss);
        let eased = expected_counts(&units, &widened, &model, skill).get(ManiaJudgement::Miss);

        assert!(
            plain_misses > 0.0,
            "the probe skill should produce timing misses, got {plain_misses}"
        );

        assert!(
            eased < plain_misses,
            "widening the miss window should reduce predicted misses: {eased} vs {plain_misses}"
        );
    }

    /// Standard normal CDF.
    fn phi(z: f64) -> f64 {
        0.5 * erfc(-z / std::f64::consts::SQRT_2)
    }

    /// Judgement counts for a player whose timing error is centred on `offset`
    /// rather than zero: `X ~ N(offset, sigma)`.
    fn offset_score(windows: &ManiaHitWindows, sigma: f64, offset: f64, total: u32) -> [u32; 6] {
        let mut probabilities = [0.0; 6];

        for judgement in ManiaJudgement::ALL {
            let (lower, upper) = windows.band(judgement);
            // Both sides of zero, since the band is on |X|.
            let positive = phi((upper - offset) / sigma) - phi((lower - offset) / sigma);
            let negative = phi((-lower - offset) / sigma) - phi((-upper - offset) / sigma);
            probabilities[judgement as usize] = positive + negative;
        }

        ExpectedCounts(probabilities.map(|p| p * f64::from(total))).round_to_hits(total)
    }

    /// An offset score is not a lower-skill score, and the fit cannot tell the
    /// difference. Recorded because it is the model's main blind spot: the surface
    /// assumes errors are centred on zero, and a player who is not gets read as
    /// less precise than they are.
    #[test]
    fn a_constant_offset_biases_skill_downward() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 1300;
        let units = uniform_units(5.0, notes);
        let sigma = model.sigma(5.0, 6.0);

        let centred = offset_score(&windows, sigma, 0.0, notes as u32);
        let baseline = skill_for_counts(&centred, &units, &windows, &model);

        let mut previous = baseline;

        // Same precision throughout — only the centre moves — so every drop here is
        // the offset being misread as imprecision.
        for &offset in &[10.0, 20.0, 30.0, 45.0] {
            let score = offset_score(&windows, sigma, offset, notes as u32);
            let estimate = skill_for_counts(&score, &units, &windows, &model);

            assert!(
                estimate < previous,
                "a larger offset should not raise skill: {offset}ms gave {estimate}, \
                 previous {previous}"
            );

            previous = estimate;
        }

        // The magnitude matters for calibration: a 20 ms offset is a mid-sized audio
        // desync and already costs a fifth of the estimate.
        let twenty = offset_score(&windows, sigma, 20.0, notes as u32);
        let ratio = skill_for_counts(&twenty, &units, &windows, &model) / baseline;

        assert!(
            ratio < 0.85,
            "a 20ms offset should visibly depress the estimate, got {ratio}"
        );
    }

    /// The offset case is what the plausibility flag exists for, since the skill it
    /// returns is not trustworthy.
    #[test]
    fn a_large_offset_is_flagged_as_implausible() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 1300;
        let units = uniform_units(5.0, notes);
        let sigma = model.sigma(5.0, 6.0);

        // Small offsets are within ordinary variation and should pass; the flag is
        // not meant to police every desynced setup.
        for &offset in &[0.0, 5.0, 10.0] {
            let score = offset_score(&windows, sigma, offset, notes as u32);
            let fit = fit_with_quality(&score, &units, &windows, &model);

            assert!(
                fit.is_plausible(),
                "a {offset}ms offset should still fit, G_timing {}",
                fit.g_timing
            );
        }

        for &offset in &[20.0, 30.0, 45.0] {
            let score = offset_score(&windows, sigma, offset, notes as u32);
            let fit = fit_with_quality(&score, &units, &windows, &model);

            assert!(
                !fit.is_plausible(),
                "a {offset}ms offset should be flagged, G_timing {}",
                fit.g_timing
            );
            // The misses are untouched, so the signal has to come from the shape.
            assert!(
                fit.excess_misses < 1.0,
                "an offset score drops no notes, got {} excess misses",
                fit.excess_misses
            );
        }
    }

    /// Dropped notes must not be mistaken for an unexplainable score. This is the
    /// case that motivated splitting `g_timing` out of `g_statistic`: the miss
    /// channel's expected count is small, so any excess dominates the combined
    /// figure even though the skill estimate is unaffected.
    #[test]
    fn a_lag_spike_stays_plausible_and_barely_moves_skill() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 1300;
        let units = uniform_units(5.0, notes);

        let clean = synthetic_score(&units, &windows, &model, 6.0, notes as u32);
        let baseline = skill_for_counts(&clean, &units, &windows, &model);

        for &lost in &[20u32, 50, 100, 200] {
            let mut score = clean;
            let moved = lost.min(score[ManiaJudgement::Perfect as usize]);
            score[ManiaJudgement::Perfect as usize] -= moved;
            score[ManiaJudgement::Miss as usize] += moved;

            let fit = fit_with_quality(&score, &units, &windows, &model);

            assert!(
                fit.is_plausible(),
                "{lost} dropped notes should not read as an unexplainable score, \
                 G_timing {} (combined G {})",
                fit.g_timing,
                fit.g_statistic
            );

            let shift = (fit.skill - baseline).abs() / baseline;

            assert!(
                shift < 0.10,
                "{lost} dropped notes moved skill by {:.1}%",
                shift * 100.0
            );

            // The drop is still reported, just not as a fit failure.
            assert!(
                fit.excess_misses > f64::from(lost) - 2.0,
                "expected ~{lost} excess misses, got {}",
                fit.excess_misses
            );
        }
    }

    /// The precise reason the combined statistic cannot be the threshold.
    #[test]
    fn dropped_notes_dominate_the_combined_statistic_but_not_the_timing_one() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let notes = 1300;
        let units = uniform_units(5.0, notes);

        let mut score = synthetic_score(&units, &windows, &model, 6.0, notes as u32);
        score[ManiaJudgement::Perfect as usize] -= 50;
        score[ManiaJudgement::Miss as usize] += 50;

        let fit = fit_with_quality(&score, &units, &windows, &model);

        assert!(
            fit.g_statistic > 100.0,
            "expected the miss channel to blow up the combined figure, got {}",
            fit.g_statistic
        );
        assert!(
            fit.g_timing < 5.0,
            "the timing bands are untouched and should still fit, got {}",
            fit.g_timing
        );
    }

    /// The scale has to span the whole userbase, from someone who cannot play the
    /// map at all to someone who SSes it comfortably — and it has to do so on maps
    /// of any difficulty, since a single skill number is compared against `d_all`.
    #[test]
    fn the_skill_scale_spans_beginner_to_ss_at_every_difficulty() {
        let windows = od9_windows();
        let timing = ErrorModel {
            slip_rate: 0.0,
            ..ErrorModel::default()
        };
        let notes = 1000;

        for &difficulty in &[2.0, 5.0, 8.0, 12.0, 20.0] {
            let units = uniform_units(difficulty, notes);

            let accuracy =
                |skill: f64| expected_counts(&units, &windows, &timing, skill).custom_accuracy();

            // Both ends must actually be reachable within the search bracket, or the
            // fit would be pinned for a whole class of real players.
            assert!(
                accuracy(SKILL_MIN) < 0.01,
                "d={difficulty}: SKILL_MIN should be unplayable, got {}",
                accuracy(SKILL_MIN)
            );
            assert!(
                accuracy(SKILL_MAX) > 0.9999,
                "d={difficulty}: SKILL_MAX should SS, got {}",
                accuracy(SKILL_MAX)
            );

            // And the interesting range has to sit at a consistent *ratio* to
            // difficulty, so that one scale means the same thing on a 2-star map and
            // a 20-star one.
            assert!(
                accuracy(0.5 * difficulty) < 0.80,
                "d={difficulty}: half difficulty should not earn pp, got {}",
                accuracy(0.5 * difficulty)
            );
            assert!(
                accuracy(SKILL_SATURATION_RATIO * difficulty) > 0.999,
                "d={difficulty}: the saturation ratio should be an SS, got {}",
                accuracy(SKILL_SATURATION_RATIO * difficulty)
            );
        }
    }

    /// The cost of conditioning misses out, recorded so it cannot be rediscovered as
    /// a surprise: at the very bottom the fit stops resolving anything.
    #[test]
    fn the_fit_reports_a_floor_rather_than_a_measurement_for_a_beginner() {
        let windows = od9_windows();
        let model = ErrorModel::default();
        let units = uniform_units(5.0, 1000);

        // Scores differing only in miss count are indistinguishable to a conditional
        // fit, because the surviving band shares have converged.
        let mut previous: Option<f64> = None;

        for &misses in &[950u32, 800, 600] {
            let hits = 1000 - misses;
            let mut score = [0u32; 6];
            // Spread the hits on the window-width ratios the shares converge to.
            score[ManiaJudgement::Meh as usize] = (f64::from(hits) * 0.193).round() as u32;
            score[ManiaJudgement::Ok as usize] = (f64::from(hits) * 0.241).round() as u32;
            score[ManiaJudgement::Good as usize] = (f64::from(hits) * 0.265).round() as u32;
            score[ManiaJudgement::Great as usize] = (f64::from(hits) * 0.169).round() as u32;
            score[ManiaJudgement::Perfect as usize] =
                hits - score[0] - score[1] - score[2] - score[3];
            score[ManiaJudgement::Miss as usize] = misses;

            let fit = fit_with_quality(&score, &units, &windows, &model);

            if let Some(previous) = previous {
                assert!(
                    (fit.skill - previous).abs() / previous < 0.05,
                    "the fit should not resolve these apart, got {} vs {previous}",
                    fit.skill
                );
            }

            previous = Some(fit.skill);
        }

        // A total beginner bottoms out, and says so.
        let mut all_misses = [0u32; 6];
        all_misses[ManiaJudgement::Miss as usize] = 1000;

        let fit = fit_with_quality(&all_misses, &units, &windows, &model);

        assert!(
            !fit.is_identifiable(),
            "an all-miss score should not claim a measured skill, got {}",
            fit.skill
        );

        // Whereas an ordinary score is identifiable, so the flag is not simply always
        // false.
        let ordinary = synthetic_score(&units, &windows, &model, 4.0, 1000);
        let fit = fit_with_quality(&ordinary, &units, &windows, &model);

        assert!(
            fit.is_identifiable(),
            "a mid-skill score should be identifiable, got {}",
            fit.skill
        );
    }
}
