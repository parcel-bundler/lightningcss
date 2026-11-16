#!/usr/bin/env python3
# Generate synthetic stylesheets shaped like common real-world CSS, for
# measuring output size changes (see scripts/size-check.sh). Deterministic.
#
#   python3 scripts/size-corpus.py <kind>     print one stylesheet
#   python3 scripts/size-corpus.py --all DIR  write every kind to DIR/<kind>.css
#
# Kinds:
#   tw4           one Tailwind-v4-shaped build (layer statement, theme/base/
#                 components/utilities layers, nested @media, @property, @keyframes)
#   tw4x3         an app and two prebuilt Tailwind-v4 libraries concatenated
#   components    per-component bundles: repeated @layer blocks with @keyframes
#                 and unlayered @media in between
#   atomic        atomic CSS: one @layer block per class, @property/@keyframes
#                 interleaved (the case that regressed when 32aecd9 stopped
#                 merging layer blocks)
#   atomic-media  atomic CSS with responsive variants in @media{@layer{}}

import os, sys

props = [("display","flex"),("display","grid"),("display","block"),("padding","%drem"),("margin","%dpx"),("color","var(--color-c%d)"),("gap","%dpx"),("width","%d%%"),("font-size","%dpx"),("opacity","0.%d")]
def util(i):
    p,v = props[i % len(props)]
    if "%" in v and v.count("%")>=1 and "%%" not in v.replace("%d",""):
        v = v % (i%9+1) if "%d" in v else v
    elif "%d" in v: v = v.replace("%d", str(i%9+1)).replace("%%","%")
    return p, v.replace("%%","%")
def theme():
    return "@layer theme{:root,:host{" + "".join("--color-c%d:#%02x%02x%02x;"%(i,i*4,255-i*4,(i*37)%256) for i in range(60)) + "--font-sans:ui-sans-serif,system-ui,sans-serif;}}\n"
def base():
    return "@layer base{*,::after,::before{box-sizing:border-box;border:0 solid;margin:0;padding:0}html,:host{line-height:1.5;font-family:var(--font-sans)}h1,h2,h3{font-size:inherit;font-weight:inherit}a{color:inherit;text-decoration:inherit}img,svg,video{display:block;max-width:100%}button,input{font:inherit;color:inherit}}\n"
def utilities(prefix, n):
    out = "@layer utilities{"
    for i in range(n):
        p,v = util(i)
        out += ".%s-u%d{%s:%s}" % (prefix, i, p, v)
        if i % 5 == 0:
            out += ".sm\\:%s-u%d{@media (width>=40rem){%s:%s}}" % (prefix, i, p, v)
        if i % 7 == 0:
            out += ".hover\\:%s-u%d{&:hover{@media (hover:hover){%s:%s}}}" % (prefix, i, p, v)
    return out + "}\n"
def properties_tail(n):
    s = "".join('@property --tw-p%d{syntax:"*";inherits:false}\n' % i for i in range(n))
    s += "@keyframes spin{to{transform:rotate(360deg)}}\n"
    s += "@layer properties{@supports ((-webkit-hyphens:none) and (not (margin-trim:inline))) or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,::before,::after,::backdrop{" + "".join("--tw-p%d:initial;"%i for i in range(n)) + "}}}\n"
    return s
def tw4(prefix="tw", n=400):
    return "@layer properties;\n@layer theme, base, components, utilities;\n" + theme() + base() + "@layer components{.btn{padding:.5rem 1rem;border-radius:.25rem}}\n" + utilities(prefix, n) + properties_tail(20)

def tw4x3():
    # An app and two prebuilt Tailwind-v4 libraries concatenated by a bundler.
    return tw4("app", 400) + tw4("lib1", 150) + tw4("lib2", 150)

def components():
    # Per-component CSS files bundled: each has a @layer components block, with
    # some @keyframes / unlayered @media in between.
    s = "@layer reset, base, components, utilities;\n@layer reset{*{margin:0}}\n"
    for c in range(80):
        s += "@layer components{.c%d{display:flex;padding:%dpx}.c%d__title{font-size:%dpx;color:#333}}\n" % (c, c%8, c, 12+c%6)
        if c % 3 == 0: s += "@keyframes c%d-in{from{opacity:0}to{opacity:1}}\n" % c
        if c % 4 == 0: s += "@media (min-width:768px){.c%d-unl{display:none}}\n" % c
        if c % 5 == 0: s += "@layer utilities{.u%d{margin:%dpx}}\n" % (c, c)
    return s

def atomic():
    # Atomic CSS: each class in its own layer block; @property / @keyframes
    # emitted next to the class that needs them.
    s = "@layer base, atoms;\n"
    for i in range(500):
        p, v = util(i)
        s += "@layer atoms{.a%d{%s:%s}}\n" % (i, p, v)
        if i % 10 == 0: s += '@property --a%d{syntax:"<length>";inherits:false;initial-value:0px}\n' % i
        if i % 25 == 0: s += "@keyframes k%d{to{opacity:0}}\n" % i
    return s

def atomic_media():
    # Atomic CSS with responsive variants as @media containing the same layer.
    s = "@layer base, atoms;\n"
    for i in range(500):
        p, v = util(i)
        s += "@layer atoms{.a%d{%s:%s}}\n" % (i, p, v)
        if i % 4 == 0: s += "@media (min-width:640px){@layer atoms{.sm_a%d{%s:%s}}}\n" % (i, p, v)
    return s

KINDS = {"tw4": tw4, "tw4x3": tw4x3, "components": components, "atomic": atomic, "atomic-media": atomic_media}

if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--all":
        os.makedirs(sys.argv[2], exist_ok=True)
        for name, fn in KINDS.items():
            with open(os.path.join(sys.argv[2], name + ".css"), "w") as f:
                f.write(fn())
    elif len(sys.argv) == 2 and sys.argv[1] in KINDS:
        sys.stdout.write(KINDS[sys.argv[1]]())
    else:
        sys.exit("usage: size-corpus.py <%s> | --all DIR" % "|".join(KINDS))
