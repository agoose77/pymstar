from __future__ import annotations

__all__ = ("mstar", "TARGETS")

from . import _ext

import atexit
import contextlib
import importlib.resources
import os
import re
from typing_extensions import Literal


def _register_database():
    # Clean this up on exit!
    _stack = contextlib.ExitStack()
    atexit.register(_stack.close)

    # Ensure that MSTAR has the database available
    _database_path = _stack.enter_context(
        importlib.resources.path(__name__, "msdbs1.d")
    )
    os.environ['__MSTAR_DATABASE_PATH'] = os.fspath(_database_path)
    return _database_path


_DATABASE_PATH = _register_database()


def _parse_database_targets(database_path) -> list[tuple[int, str]]:
    with open(database_path) as f:
        for match in re.finditer(r"^ +(\d+) +\'([^']+)'$", f.read(),
                                 flags=re.MULTILINE):
            yield match[2], int(match[1])


_TARGET_TO_ID = dict(_parse_database_targets(_DATABASE_PATH))

TARGETS = tuple(_TARGET_TO_ID)


def mstar(target: str, z: int, energy: float, mode: Literal['a', 'b', 'c', 'd', 'g', 'h'] = 'b') -> float:
    """

    :param target: literal of target, see {data}`TARGETS` or <https://www-nds.iaea.org/stopping/MstarWWW/MSTARInstr.html>
    :param z: atomic number of projectile
    :param energy: energy of projectile (MeV/u)
    :param mode: MSTAR mode, see <https://www-nds.iaea.org/stopping/MstarWWW/MSTARInstr.html>
    :return: the stopping power (MeV cm2 /mg)
    """
    try:
        target_id = _TARGET_TO_ID[target]
    except KeyError:
        raise ValueError(f"Invalid target {target}")

    ste, rc = _ext.mstar1(target_id, mode, z, energy, "")
    if rc == 0:
        return ste
    elif rc == 30:
        raise ValueError("ALL ENERGIES OUTSIDE ALLOWED RANGE")
    else:
        raise RuntimeError(_ext.msemsg(rc).decode())
