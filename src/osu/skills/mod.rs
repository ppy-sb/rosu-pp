mod aim;
mod flashlight;
mod speed;
mod traits;

use crate::osu::difficulty_object::OsuDifficultyObject;

pub(crate) use self::{
    aim::Aim,
    flashlight::Flashlight,
    speed::Speed,
    traits::{OsuStrainSkill, Skill, StrainSkill},
};

#[derive(Clone, Debug)]
pub(crate) struct Skills {
    pub aim: Aim,
    pub aim_no_sliders: Aim,
    pub aim_rx: Aim,
    pub speed: Speed,
    pub speed_rx: Speed,
    pub flashlight: Flashlight,
}

impl Skills {
    pub(crate) fn new(
        mods: u32,
        radius: f32,
        time_preempt: f64,
        time_fade_in: f64,
        hit_window: f64,
    ) -> Self {
        Self {
            aim: Aim::new(true, false, hit_window),
            aim_no_sliders: Aim::new(false, false, hit_window),
            aim_rx: Aim::new(true, true, hit_window),
            speed: Speed::new(hit_window, false),
            speed_rx: Speed::new(hit_window, true),
            flashlight: Flashlight::new(mods, radius, time_preempt, time_fade_in),
        }
    }

    pub(crate) fn process(
        &mut self,
        curr: &OsuDifficultyObject<'_>,
        diff_objects: &[OsuDifficultyObject<'_>],
    ) {
        <Aim as Skill>::process(&mut self.aim, curr, diff_objects);
        <Aim as Skill>::process(&mut self.aim_no_sliders, curr, diff_objects);
        <Aim as Skill>::process(&mut self.aim_rx, curr, diff_objects);
        <Speed as Skill>::process(&mut self.speed, curr, diff_objects);
        <Speed as Skill>::process(&mut self.speed_rx, curr, diff_objects);
        <Flashlight as Skill>::process(&mut self.flashlight, curr, diff_objects);
    }
}

fn previous<'map, 'objects>(
    diff_objects: &'objects [OsuDifficultyObject<'map>],
    curr: usize,
    backwards_idx: usize,
) -> Option<&'objects OsuDifficultyObject<'map>> {
    curr.checked_sub(backwards_idx + 1)
        .and_then(|idx| diff_objects.get(idx))
}

fn previous_start_time(
    diff_objects: &[OsuDifficultyObject<'_>],
    curr: usize,
    backwards_idx: usize,
) -> f64 {
    previous(diff_objects, curr, backwards_idx).map_or(0.0, |h| h.start_time)
}

fn next<'map, 'objects>(
    diff_objects: &'objects [OsuDifficultyObject<'map>],
    curr: usize,
    forwards_idx: usize,
) -> Option<&'objects OsuDifficultyObject<'map>> {
    diff_objects.get(curr + (forwards_idx + 1))
}
