package com.mogproject.mogami.bench

trait TestData {
  val recordSfen01: String = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0"
  val recordSfen02: String = Seq(
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0",
    " 7g7f 3c3d 2g2f 4a3b 6i7h 8c8d 2f2e 2b8h+ 7i8h 3a2b",
    " 3i3h 2b3c 3g3f 7a7b 5i6h 6c6d 3h3g 8d8e 3g4f 8e8f",
    " 8g8f 8b8f 2e2d 2c2d 8i7g 8f8b 3f3e 3d3e 4f3e 7c7d",
    " 3e2d 3c2d 2h2d P*2c 2d2f 7d7e P*8c 7b8c B*6c B*7d",
    " 6c1h+ 7d4g+ 4i5h 4g1d"
  ).mkString
  val recordSfen03: String = Seq(
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0",
    " 7g7f 8c8d 8h7g 3c3d 7i6h 4a3b 6g6f 3a4b 6h6g 4b3c",
    " 2h8h 2b3a 5i4h 5a4a 4h3h 7a7b 3h2h 5c5d 1i1h 1c1d",
    " 2h1i 6a5b 3i2h 4c4d 4i3i 5b4c 6f6e 3c2d 4g4f 2a3c",
    " 6g5f 6c6d 4f4e 4d4e 6e6d 3a6d 8h6h P*6c P*4d 4c4b",
    " 5f6e 6d5e 6e5d 5e7g+ 8i7g 4e4f 6i5h B*4g 5h4g 4f4g+",
    " B*7a G*3h 7a8b+ 3h2h 6h2h 3c2e R*7a P*5a B*6b 4a3a",
    " 7a5a+ S*4a 8b7b 4g3g 2h9h 3g2g 7b6c P*5b 4d4c+ 2e3g+",
    " 4c4b 3b4b 2i3g 2g3g P*2h P*4c P*3b 3a3b P*4d 4c4d",
    " 6b4d+ 2d3c G*4c 4b4c 5d4c+ 3b2b 4d3c 2b1c N*2e 1c1b",
    " G*2b"
  ).mkString
}
