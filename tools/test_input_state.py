#!/usr/bin/env python3
"""Focused regression tests for the replay input-state analysis."""

import unittest

from input_state import fit_recovery_curve


class RecoveryCurveFitTests(unittest.TestCase):
    def test_reproduces_original_285_replay_fit(self) -> None:
        # Exact rounded bin data used by the original temporary fitter. Keeping this
        # fixture here proves the published curve can be regenerated from its inputs.
        points = [
            (115.0, 13.49, 17097),
            (145.0, 5.52, 59522),
            (175.0, 3.55, 59728),
            (210.0, 1.05, 56494),
            (255.0, 0.10, 65080),
            (310.0, -2.82, 82518),
            (380.0, -3.05, 55439),
            (470.0, -2.95, 39808),
            (585.0, -3.27, 26597),
            (750.0, -2.96, 19162),
        ]

        amplitude, tau, plateau, rmse = fit_recovery_curve(points)

        self.assertAlmostEqual(amplitude, 73.12, delta=0.02)
        self.assertAlmostEqual(tau, 72.40, delta=0.02)
        self.assertAlmostEqual(plateau, -3.19, delta=0.01)
        self.assertAlmostEqual(rmse, 0.73, delta=0.01)

    def test_rejects_an_underidentified_fit(self) -> None:
        with self.assertRaisesRegex(ValueError, "at least three"):
            fit_recovery_curve([(100.0, 1.0, 20), (200.0, 0.0, 20)])


if __name__ == "__main__":
    unittest.main()
