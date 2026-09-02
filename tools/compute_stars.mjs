// Compute sunny mania star rating for (map_id, mods_bitflag) pairs, using the
// sunny wasm build already vendored for the server (refactor/osu-server-ts).
// Needed when a fixture's mods/stars have to be read from a raw CSV export
// rather than the (possibly stale) docker-mysql mirror.
//
// Usage: node tools/compute_stars.mjs <pairs.tsv> <maps_dir> > out.tsv
//   pairs.tsv: mapid<TAB>mods_bitflag (one per line, no header)
// Output: mapid<TAB>mods<TAB>stars
import { readFileSync } from 'fs'
import { createRequire } from 'module'
const require = createRequire(import.meta.url)
const { Beatmap, SunnyManiaDifficulty } = require(
  '/Users/Shared/git/ppy-sb/refactor/osu-server-ts/node_modules/rosu-pp-js-sunny',
)

const [pairsPath, mapsDir] = process.argv.slice(2)
const lines = readFileSync(pairsPath, 'utf8').trim().split('\n').filter(Boolean)

const mapCache = new Map()

for (const line of lines) {
  const [mapid, modsStr] = line.split('\t')
  const mods = parseInt(modsStr, 10)
  try {
    let buf = mapCache.get(mapid)
    if (!buf) {
      buf = readFileSync(`${mapsDir}/${mapid}.osu`)
      mapCache.set(mapid, buf)
    }
    const map = new Beatmap(buf)
    const diff = new SunnyManiaDifficulty({ mods, lazer: false })
    const attrs = diff.calculate(map)
    console.log(`${mapid}\t${mods}\t${attrs.stars}`)
    attrs.free()
    diff.free()
    map.free()
  }
  catch (e) {
    console.error(`! ${mapid} mods=${mods}: ${e.message}`)
  }
}
