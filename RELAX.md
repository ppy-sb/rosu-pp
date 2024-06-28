### Terms

strain_time: the real delta time(ms) between two hit objects. e.g. 300BPM Jump: 60 / 300 * 1/2 = 100, 200BPM Stream: 60 / 200 * 1/4 = 75.

hit_window: combination of strain_time and overall_difficulty, representing the hit range that players should follow.

### Ideas

1. SPD should be considered in AIM skills, instead of zero or a individual skills.
2. Strain_time should be nerfed, since relax is not so that sensitive with velocity.
3. We don't want to consider wide angle stream as a hard pattern, since it's much easier in relax.

### Changes

1. Including all post calculation from Akatsuki PP.
2. Removing all post relax calculation from PPY Formula.