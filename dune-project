(lang dune 2.9)
(name snoke)
(version "0.3-pre")
(generate_opam_files true)
(license GPL-3.0-or-later)
(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(source (github sanette/snoke))


(package
 (name snoke)
 (synopsis "Snóke is a good old Snake game with new ideas")
 (depends bogue) ; (bogue (>= 20221008) in fact, but this version is
                 ; not in opam right now, so we do this in order to
                 ; make the CI happy.)
)
