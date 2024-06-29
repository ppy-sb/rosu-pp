### Terms

strain_time: the real delta time(ms) between two hit objects. e.g. 300BPM Jump: 60 / 300 * 1/2 = 100, 200BPM Stream: 60 / 200 * 1/4 = 75.

hit_window: overall_difficulty, representing the hit range that players should follow.

### Ideas

1. Nerf wide angle stream (circle stream), since it's much easier in relax.
1. Nerf velocity bonus in acute angle calculation, since relax is not so that sensitive with velocity.
1. Include relax rhythm evaluator
1. Nerf velocity bonus in wide angle stream calculation, since relax is not so that sensitive with velocity.

### Changes

1. Remove all post calculation from ppy formula.
1. Include all post calculation from akatsuki formula.
1. Include relax rhythm evaluator.
1. Adjust multipliers to bonus VELOCITY_CHANGE and SLIDER.
1. Nerf round stream (aka. wide angle stream).
1. Nerf velocity bonus in acute angle.
1. Bonus scores that doesn't enable DT.

### Ongoing Changes

- Nerf velocity bonus in round stream.