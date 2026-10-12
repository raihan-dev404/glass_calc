import 'dart:math' as math;
import 'package:flutter/foundation.dart';
import 'dart:convert';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:flutter/material.dart';
import 'dart:ui' show ImageFilter;
import 'package:flutter/services.dart';
import 'package:url_launcher/url_launcher.dart';

/// Thrown when an expression can't be evaluated (bad syntax, divide by zero…).
class CalcException implements Exception {
  final String message;
  const CalcException(this.message);

  @override
  String toString() => message;
}

enum _T { number, op, lparen, rparen, func, constant, bang, percent }

class _Tok {
  final _T type;
  final String text;
  final double value;
  const _Tok(this.type, this.text, [this.value = 0]);
}

/// A small, correct expression engine.
///
/// Supports: + - × ÷ ^ ( ) unary minus, implicit multiplication (2π, 3(4)),
/// postfix % and !, π, e, √, sin cos tan asin acos atan ln log.
/// Missing closing brackets are closed automatically so the live preview
/// works while the user is still typing (e.g. `sin(30`).
class CalcEngine {
  CalcEngine._();

  static const _funcNames = [
    'asin',
    'acos',
    'atan',
    'sin',
    'cos',
    'tan',
    'ln',
    'log',
  ];

  /// Evaluates [expression]. Throws [CalcException] on any problem.
  static double evaluate(String expression, {bool isRad = true}) {
    final tokens = _tokenize(expression);
    if (tokens.isEmpty) throw const CalcException('Empty');
    final parser = _Parser(tokens, isRad);
    final value = parser.parseAll();
    if (value.isNaN || value.isInfinite) {
      throw const CalcException('Invalid input');
    }
    return value;
  }

  /// Turns a result into a clean string.
  /// Up to 12 significant digits; very large/small numbers use `a×10^b`
  /// (which the evaluator can read back in).
  static String format(double v) {
    if (v.isNaN || v.isInfinite) throw const CalcException('Invalid input');
    if (v == 0) return '0';
    final a = v.abs();

    if (a >= 1e15 || a < 1e-6) {
      final parts = v.toStringAsExponential(9).split('e');
      var mant = parts[0];
      if (mant.contains('.')) {
        mant = mant.replaceFirst(RegExp(r'0+$'), '');
        mant = mant.replaceFirst(RegExp(r'\.$'), '');
      }
      final exp = int.parse(parts[1]);
      return '$mant×10^$exp';
    }

    final r = double.parse(v.toStringAsPrecision(12));
    if (r == r.truncateToDouble()) return r.toStringAsFixed(0);

    final magnitude = (math.log(r.abs()) / math.ln10).floor();
    final int decimals = (11 - magnitude).clamp(0, 20).toInt();
    var s = r.toStringAsFixed(decimals);
    if (s.contains('.')) {
      s = s.replaceFirst(RegExp(r'0+$'), '');
      s = s.replaceFirst(RegExp(r'\.$'), '');
    }
    return s;
  }

  // ---------------------------------------------------------------- lexer

  static List<_Tok> _tokenize(String s) {
    final out = <_Tok>[];
    var i = 0;
    while (i < s.length) {
      final c = s[i];

      if (c == ' ') {
        i++;
        continue;
      }

      // numbers
      if (_isDigit(c) || c == '.') {
        final start = i;
        var dots = 0;
        while (i < s.length && (_isDigit(s[i]) || s[i] == '.')) {
          if (s[i] == '.') dots++;
          i++;
        }
        final text = s.substring(start, i);
        final n = double.tryParse(text);
        if (dots > 1 || n == null) throw const CalcException('Invalid number');
        out.add(_Tok(_T.number, text, n));
        continue;
      }

      // operators
      if (c == '+') {
        out.add(const _Tok(_T.op, '+'));
        i++;
        continue;
      }
      if (c == '-' || c == '−') {
        out.add(const _Tok(_T.op, '-'));
        i++;
        continue;
      }
      if (c == '×' || c == '*') {
        out.add(const _Tok(_T.op, '*'));
        i++;
        continue;
      }
      if (c == '÷' || c == '/') {
        out.add(const _Tok(_T.op, '/'));
        i++;
        continue;
      }
      if (c == '^') {
        out.add(const _Tok(_T.op, '^'));
        i++;
        continue;
      }
      if (c == '(') {
        out.add(const _Tok(_T.lparen, '('));
        i++;
        continue;
      }
      if (c == ')') {
        out.add(const _Tok(_T.rparen, ')'));
        i++;
        continue;
      }
      if (c == '!') {
        out.add(const _Tok(_T.bang, '!'));
        i++;
        continue;
      }
      if (c == '%') {
        out.add(const _Tok(_T.percent, '%'));
        i++;
        continue;
      }
      if (c == 'π') {
        out.add(_Tok(_T.constant, 'π', math.pi));
        i++;
        continue;
      }
      if (c == '√') {
        out.add(const _Tok(_T.func, 'sqrt'));
        i++;
        continue;
      }

      // names (longest first so "asin" wins over "sin")
      String? matched;
      for (final name in _funcNames) {
        if (s.startsWith(name, i)) {
          matched = name;
          break;
        }
      }
      if (matched != null) {
        out.add(_Tok(_T.func, matched));
        i += matched.length;
        continue;
      }
      if (c == 'e') {
        out.add(_Tok(_T.constant, 'e', math.e));
        i++;
        continue;
      }

      throw CalcException('Unknown symbol "$c"');
    }
    return out;
  }

  static bool _isDigit(String c) {
    final u = c.codeUnitAt(0);
    return u >= 48 && u <= 57;
  }
}

// ------------------------------------------------------------------ parser
//
//   expr    := term (('+' | '-') term)*
//   term    := unary (('×' | '÷') unary | <implicit> unary)*
//   unary   := ('-' | '+') unary | power
//   power   := postfix ('^' unary)?          (right-associative)
//   postfix := primary ('!' | '%')*
//   primary := number | constant | '(' expr ')' | func '(' expr ')' | func unary

class _Parser {
  final List<_Tok> t;
  final bool isRad;
  int pos = 0;

  _Parser(this.t, this.isRad);

  _Tok? get _peek => pos < t.length ? t[pos] : null;

  bool _isOp(String text) {
    final p = _peek;
    return p != null && p.type == _T.op && p.text == text;
  }

  double parseAll() {
    final v = _expr();
    if (pos != t.length) throw const CalcException('Check your brackets');
    return v;
  }

  double _expr() {
    var v = _term();
    while (_isOp('+') || _isOp('-')) {
      final plus = _peek!.text == '+';
      pos++;
      final start = pos;
      var r = _term();
      // "200 + 10%" means 200 + 10% *of 200*, like a phone calculator.
      if (pos - start == 2 &&
          t[start].type == _T.number &&
          t[start + 1].type == _T.percent) {
        r = v * r;
      }
      v = plus ? v + r : v - r;
    }
    return v;
  }

  bool _startsImplicit(_Tok? p) {
    if (p == null) return false;
    return p.type == _T.number ||
        p.type == _T.lparen ||
        p.type == _T.func ||
        p.type == _T.constant;
  }

  double _term() {
    var v = _unary();
    while (true) {
      if (_isOp('*')) {
        pos++;
        v = v * _unary();
      } else if (_isOp('/')) {
        pos++;
        final d = _unary();
        if (d == 0) throw const CalcException('Cannot divide by zero');
        v = v / d;
      } else if (_startsImplicit(_peek)) {
        v = v * _unary();
      } else {
        break;
      }
    }
    return v;
  }

  double _unary() {
    if (_isOp('-')) {
      pos++;
      return -_unary();
    }
    if (_isOp('+')) {
      pos++;
      return _unary();
    }
    return _power();
  }

  double _power() {
    final base = _postfix();
    if (_isOp('^')) {
      pos++;
      final exp = _unary();
      if (base == 0 && exp < 0) {
        throw const CalcException('Cannot divide by zero');
      }
      final r = math.pow(base, exp).toDouble();
      if (r.isNaN || r.isInfinite) throw const CalcException('Invalid input');
      return r;
    }
    return base;
  }

  double _postfix() {
    var v = _primary();
    while (_peek != null &&
        (_peek!.type == _T.bang || _peek!.type == _T.percent)) {
      if (_peek!.type == _T.bang) {
        v = _factorial(v);
      } else {
        v = v / 100;
      }
      pos++;
    }
    return v;
  }

  double _primary() {
    final tok = _peek;
    if (tok == null) throw const CalcException('Incomplete expression');
    pos++;
    switch (tok.type) {
      case _T.number:
      case _T.constant:
        return tok.value;
      case _T.lparen:
        final v = _expr();
        _closeParen();
        return v;
      case _T.func:
        double arg;
        if (_peek != null && _peek!.type == _T.lparen) {
          pos++;
          arg = _expr();
          _closeParen();
        } else {
          arg = _unary();
        }
        return _apply(tok.text, arg);
      default:
        throw const CalcException('Incomplete expression');
    }
  }

  // A missing ")" at the very end is fine (auto-closed).
  void _closeParen() {
    if (_peek != null && _peek!.type == _T.rparen) {
      pos++;
    } else if (_peek != null) {
      throw const CalcException('Check your brackets');
    }
  }

  double _factorial(double x) {
    if (x < 0 || x != x.truncateToDouble() || x > 170) {
      throw const CalcException('Factorial needs a whole number 0–170');
    }
    var r = 1.0;
    for (var i = 2; i <= x.toInt(); i++) {
      r *= i;
    }
    return r;
  }

  double _clean(double r) => r.abs() < 1e-12 ? 0.0 : r;

  double _apply(String fn, double x) {
    final rad = isRad ? x : x * math.pi / 180;
    switch (fn) {
      case 'sin':
        return _clean(math.sin(rad));
      case 'cos':
        return _clean(math.cos(rad));
      case 'tan':
        if (math.cos(rad).abs() < 1e-12) {
          throw const CalcException('Undefined');
        }
        return _clean(math.tan(rad));
      case 'asin':
        if (x < -1 || x > 1) throw const CalcException('Invalid input');
        return _fromRad(math.asin(x));
      case 'acos':
        if (x < -1 || x > 1) throw const CalcException('Invalid input');
        return _fromRad(math.acos(x));
      case 'atan':
        return _fromRad(math.atan(x));
      case 'ln':
        if (x <= 0) throw const CalcException('Invalid input');
        return math.log(x);
      case 'log':
        if (x <= 0) throw const CalcException('Invalid input');
        return math.log(x) / math.ln10;
      case 'sqrt':
        if (x < 0) throw const CalcException('Invalid input');
        return math.sqrt(x);
    }
    throw CalcException('Unknown function $fn');
  }

  double _fromRad(double r) => _clean(isRad ? r : r * 180 / math.pi);
}

class HistoryEntry {
  final String expression;
  final String result;

  const HistoryEntry({required this.expression, required this.result});

  Map<String, String> toJson() => {'e': expression, 'r': result};

  static HistoryEntry? fromJson(Object? json) {
    if (json is Map && json['e'] is String && json['r'] is String) {
      return HistoryEntry(
        expression: json['e'] as String,
        result: json['r'] as String,
      );
    }
    return null;
  }
}

/// All the calculator logic, with no UI in it (so it is easy to test).
///
/// Keys are plain ids: digits, `.`, `+ - × ÷ ^ % !`, `( )`, `AC`, `⌫`, `=`,
/// `π`, `e`, and the scientific ids `sqrt sin cos tan ln log sq recip inv rad`.
class CalcController extends ChangeNotifier {
  CalcController({
    List<HistoryEntry>? history,
    this.onHistoryChanged,
  }) : history = history ?? [];

  static const int maxHistory = 100;

  final List<HistoryEntry> history;
  final void Function(List<HistoryEntry> history)? onHistoryChanged;

  String input = '';
  String preview = '';
  String errorText = '';
  bool isRad = true;
  bool inverse = false;
  bool isEvaluated = false;

  static const _operators = ['+', '-', '×', '÷', '^'];
  static const _funcTokens = [
    'asin(',
    'acos(',
    'atan(',
    'sin(',
    'cos(',
    'tan(',
    'ln(',
    'log(',
    '√(',
  ];

  // ---------------------------------------------------------------- keys

  void handleKey(String key) {
    // Any key (except toggles) clears a previous error message.
    final isToggle = key == 'inv' || key == 'rad';
    if (errorText.isNotEmpty && !isToggle) errorText = '';

    switch (key) {
      case 'AC':
        input = '';
        preview = '';
        isEvaluated = false;
        inverse = false;
      case '⌫':
        _backspace();
      case '=':
        _equals();
      case 'inv':
        inverse = !inverse;
      case 'rad':
        isRad = !isRad;
        _updatePreview();
      case '( )':
        _brackets();
      case '+':
      case '-':
      case '×':
      case '÷':
      case '^':
        _operator(key);
      case '%':
      case '!':
        _postfixOp(key);
      case '.':
        _dot();
      case 'π':
      case 'e':
        _startFreshIfEvaluated();
        input += key;
        _updatePreview();
      case 'sin':
      case 'cos':
      case 'tan':
        _function(inverse ? 'a$key(' : '$key(');
      case 'ln':
        if (inverse) {
          _function('e^(');
        } else {
          _function('ln(');
        }
      case 'log':
        if (inverse) {
          _function('10^(');
        } else {
          _function('log(');
        }
      case 'sqrt':
        if (inverse) {
          _postfixOp('^2');
        } else {
          _function('√(');
        }
      case 'sq':
        _postfixOp('^2');
      case 'recip':
        _reciprocal();
      default:
        if (key.length == 1 && _isDigit(key)) {
          _digit(key);
        }
    }

    // One-shot Inv: it applies to the next function key only.
    if (const ['sin', 'cos', 'tan', 'ln', 'log', 'sqrt'].contains(key)) {
      inverse = false;
    }
    notifyListeners();
  }

  // ------------------------------------------------------------ helpers

  static bool _isDigit(String c) =>
      c.isNotEmpty && c.codeUnitAt(0) >= 48 && c.codeUnitAt(0) <= 57;

  bool _endsWithValue() {
    if (input.isEmpty) return false;
    final c = input[input.length - 1];
    return _isDigit(c) || c == ')' || c == 'π' || c == 'e' || c == '!' || c == '%';
  }

  bool _endsWithOperator() =>
      input.isNotEmpty && _operators.contains(input[input.length - 1]);

  void _startFreshIfEvaluated() {
    if (isEvaluated) {
      input = '';
      isEvaluated = false;
    }
  }

  /// The number currently being typed (digits and dots at the end).
  String _currentNumber() {
    var i = input.length;
    while (i > 0 && (_isDigit(input[i - 1]) || input[i - 1] == '.')) {
      i--;
    }
    return input.substring(i);
  }

  void _digit(String d) {
    _startFreshIfEvaluated();
    if (_currentNumber() == '0') {
      input = input.substring(0, input.length - 1); // no leading zeros
    }
    input += d;
    _updatePreview();
  }

  void _dot() {
    _startFreshIfEvaluated();
    final number = _currentNumber();
    if (number.contains('.')) return;
    input += number.isEmpty ? '0.' : '.';
    _updatePreview();
  }

  void _operator(String op) {
    isEvaluated = false; // keep using the result as the left side
    if (input.isEmpty) {
      if (op == '-') input = '-';
      _updatePreview();
      return;
    }
    if (input.endsWith('(')) {
      if (op == '-') input += '-';
      _updatePreview();
      return;
    }
    if (_endsWithOperator()) {
      final last = input[input.length - 1];
      // "5×-3": a minus right after × ÷ ^ is a negative sign.
      if (op == '-' && (last == '×' || last == '÷' || last == '^')) {
        input += op;
        _updatePreview();
        return;
      }
      // Otherwise replace the operator(s) we already typed.
      while (_endsWithOperator()) {
        input = input.substring(0, input.length - 1);
      }
      if (input.isEmpty && op != '-') {
        _updatePreview();
        return;
      }
    }
    input += op;
    _updatePreview();
  }

  void _postfixOp(String op) {
    if (!_endsWithValue()) return;
    isEvaluated = false;
    input += op;
    _updatePreview();
  }

  void _function(String text) {
    _startFreshIfEvaluated();
    input += text;
    _updatePreview();
  }

  void _reciprocal() {
    if (input.isEmpty) {
      input = '1÷(';
    } else {
      input = '1÷($input)';
    }
    isEvaluated = false;
    _updatePreview();
  }

  void _brackets() {
    _startFreshIfEvaluated();
    final open = '('.allMatches(input).length;
    final close = ')'.allMatches(input).length;
    if (open > close && _endsWithValue()) {
      input += ')';
    } else {
      input += '(';
    }
    _updatePreview();
  }

  void _backspace() {
    if (isEvaluated) {
      input = '';
      preview = '';
      isEvaluated = false;
      return;
    }
    if (input.isEmpty) return;
    for (final f in _funcTokens) {
      if (input.endsWith(f)) {
        input = input.substring(0, input.length - f.length);
        _updatePreview();
        return;
      }
    }
    input = input.substring(0, input.length - 1);
    _updatePreview();
  }

  void _equals() {
    if (input.isEmpty) return;
    try {
      final result = CalcEngine.format(
        CalcEngine.evaluate(input, isRad: isRad),
      );
      if (result != input) {
        history.insert(0, HistoryEntry(expression: input, result: result));
        if (history.length > maxHistory) {
          history.removeRange(maxHistory, history.length);
        }
        onHistoryChanged?.call(history);
      }
      input = result;
      preview = '';
      isEvaluated = true;
    } on CalcException catch (e) {
      preview = '';
      errorText = e.message;
    }
  }

  void _updatePreview() {
    preview = '';
    if (input.isEmpty) return;
    // A plain number needs no preview.
    if (RegExp(r'^-?[0-9.]+$').hasMatch(input)) return;
    try {
      final formatted = CalcEngine.format(
        CalcEngine.evaluate(input, isRad: isRad),
      );
      preview = formatted == input ? '' : formatted;
    } on CalcException {
      preview = ''; // still typing – stay quiet
    }
  }

  // ------------------------------------------------------------ history

  /// Puts a history value back on the screen.
  /// If you were in the middle of a sum (after an operator or "("), it is
  /// added to it; otherwise it replaces what's on screen.
  void useHistory(HistoryEntry entry, {bool useExpression = false}) {
    final text = useExpression ? entry.expression : entry.result;
    errorText = '';
    if (!isEvaluated && input.isNotEmpty && (_endsWithOperator() || input.endsWith('('))) {
      input += text;
    } else {
      input = text;
    }
    isEvaluated = false;
    _updatePreview();
    notifyListeners();
  }

  void clearHistory() {
    history.clear();
    onHistoryChanged?.call(history);
    notifyListeners();
  }

  /// What a long-press on the display should copy.
  String get copyText {
    if (isEvaluated) return input;
    if (preview.isNotEmpty) return preview;
    return input;
  }
}

/// User choices + saved history. Everything is stored on the phone only.
class AppSettings extends ChangeNotifier {
  AppSettings._(this._prefs);

  /// For tests / if storage is unavailable: works fine, just doesn't save.
  AppSettings.inMemory() : _prefs = null;

  final SharedPreferences? _prefs;

  bool isDark = true;
  int accentIndex = 0;
  bool haptics = true;
  bool liquidMotion = true;
  bool realBlur = false;
  List<HistoryEntry> history = [];

  static const _kDark = 'dark';
  static const _kAccent = 'accent';
  static const _kHaptics = 'haptics';
  static const _kMotion = 'motion';
  static const _kBlur = 'blur';
  static const _kHistory = 'history';

  static Future<AppSettings> load() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      final s = AppSettings._(prefs);
      s.isDark = prefs.getBool(_kDark) ?? true;
      s.accentIndex = prefs.getInt(_kAccent) ?? 0;
      s.haptics = prefs.getBool(_kHaptics) ?? true;
      s.liquidMotion = prefs.getBool(_kMotion) ?? true;
      s.realBlur = prefs.getBool(_kBlur) ?? false;
      final raw = prefs.getString(_kHistory);
      if (raw != null) {
        final decoded = jsonDecode(raw);
        if (decoded is List) {
          s.history = decoded
              .map(HistoryEntry.fromJson)
              .whereType<HistoryEntry>()
              .toList();
        }
      }
      return s;
    } catch (e) {
      debugPrint('Could not load settings: $e');
      return AppSettings.inMemory();
    }
  }

  void setDark(bool v) {
    isDark = v;
    _prefs?.setBool(_kDark, v);
    notifyListeners();
  }

  void setAccent(int i) {
    accentIndex = i;
    _prefs?.setInt(_kAccent, i);
    notifyListeners();
  }

  void setHaptics(bool v) {
    haptics = v;
    _prefs?.setBool(_kHaptics, v);
    notifyListeners();
  }

  void setLiquidMotion(bool v) {
    liquidMotion = v;
    _prefs?.setBool(_kMotion, v);
    notifyListeners();
  }

  void setRealBlur(bool v) {
    realBlur = v;
    _prefs?.setBool(_kBlur, v);
    notifyListeners();
  }

  void saveHistory(List<HistoryEntry> entries) {
    history = List.of(entries);
    _prefs?.setString(
      _kHistory,
      jsonEncode(entries.map((e) => e.toJson()).toList()),
    );
  }
}

class AccentOption {
  final String name;
  final Color color;
  const AccentOption(this.name, this.color);
}

/// Colours the user can pick for the operator keys and the glowing background.
const List<AccentOption> accentOptions = [
  AccentOption('Orange', Color(0xFFFF9F0A)),
  AccentOption('Blue', Color(0xFF0A84FF)),
  AccentOption('Pink', Color(0xFFFF375F)),
  AccentOption('Green', Color(0xFF30D158)),
  AccentOption('Purple', Color(0xFFBF5AF2)),
];

/// Every colour the UI needs, for one theme (dark/light) + one accent.
class GlassPalette {
  const GlassPalette({required this.dark, required this.accent});

  final bool dark;
  final Color accent;

  static const Color topBlue = Color(0xFF0A84FF);
  static const Color equalsGreen = Color(0xFF30D158);

  Color get text => dark ? Colors.white : const Color(0xFF14151C);
  Color get textDim =>
      dark ? Colors.white.withValues(alpha: 0.6) : Colors.black.withValues(alpha: 0.55);
  Color get preview => dark ? const Color(0xFF34C759) : const Color(0xFF1C9C45);
  Color get error => dark ? const Color(0xFFFF6961) : const Color(0xFFD9342B);

  List<Color> get backgroundGradient => dark
      ? const [Color(0xFF04050C), Color(0xFF0A0E1F)]
      : const [Color(0xFFEAF1FF), Color(0xFFFBEFF7)];

  /// Big soft colour blobs behind the glass. The glass blurs and bends these,
  /// which is what makes it look like glass.
  List<Color> get orbs {
    final a = dark ? 0.55 : 0.6;
    return [
      accent.withValues(alpha: a),
      const Color(0xFF0A84FF).withValues(alpha: a),
      const Color(0xFFBF5AF2).withValues(alpha: a),
      const Color(0xFF30D158).withValues(alpha: a * 0.8),
    ];
  }

  Color get sheetFill => dark
      ? const Color(0xFF12141C).withValues(alpha: 0.55)
      : Colors.white.withValues(alpha: 0.62);

  Color get menuColor => dark ? const Color(0xFF1C1C1E) : Colors.white;
}

enum ButtonType { num, top, operator, equals, scientific }

// ---------------------------------------------------------------------------
// Background: slowly drifting colour blobs for the glass to refract.
// ---------------------------------------------------------------------------

class LiquidBackground extends StatefulWidget {
  const LiquidBackground({
    super.key,
    required this.palette,
    required this.animate,
  });

  final GlassPalette palette;
  final bool animate;

  @override
  State<LiquidBackground> createState() => _LiquidBackgroundState();
}

class _LiquidBackgroundState extends State<LiquidBackground>
    with SingleTickerProviderStateMixin {
  late final AnimationController _c = AnimationController(
    vsync: this,
    duration: const Duration(seconds: 40),
  );

  @override
  void initState() {
    super.initState();
    if (widget.animate) {
      _c.repeat();
    } else {
      _c.value = 0.15;
    }
  }

  @override
  void didUpdateWidget(LiquidBackground old) {
    super.didUpdateWidget(old);
    if (widget.animate && !_c.isAnimating) {
      _c.repeat();
    } else if (!widget.animate && _c.isAnimating) {
      _c.stop();
    }
  }

  @override
  void dispose() {
    _c.dispose();
    super.dispose();
  }

  Widget _orb(Color color, double d, Alignment a) {
    return Align(
      alignment: a,
      child: Container(
        width: d,
        height: d,
        decoration: BoxDecoration(
          shape: BoxShape.circle,
          gradient: RadialGradient(
            colors: [color, color.withValues(alpha: color.a * 0.85), color.withValues(alpha: 0)],
            stops: const [0, 0.6, 1],
          ),
        ),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final p = widget.palette;
    return RepaintBoundary(
      child: LayoutBuilder(
        builder: (context, cons) {
          final base = cons.biggest.shortestSide * 1.15;
          return AnimatedBuilder(
            animation: _c,
            builder: (context, _) {
              final t = _c.value * 2 * math.pi;
              final o = p.orbs;
              return Stack(
                fit: StackFit.expand,
                children: [
                  DecoratedBox(
                    decoration: BoxDecoration(
                      gradient: LinearGradient(
                        begin: Alignment.topCenter,
                        end: Alignment.bottomCenter,
                        colors: p.backgroundGradient,
                      ),
                    ),
                  ),
                  _orb(o[0], base, Alignment(-0.9 + 0.3 * math.sin(t), -0.6 + 0.25 * math.cos(t))),
                  _orb(o[1], base * 0.9, Alignment(0.95 + 0.15 * math.cos(t), -0.05 + 0.3 * math.sin(t))),
                  _orb(o[2], base * 1.1, Alignment(-0.35 + 0.4 * math.sin(t + 2), 0.55 + 0.25 * math.cos(t + 1))),
                  _orb(o[3], base * 0.8, Alignment(0.75 + 0.25 * math.cos(t + 4), 0.95 + 0.1 * math.sin(t))),
                ],
              );
            },
          );
        },
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// The glass key.
// ---------------------------------------------------------------------------

class GlassButton extends StatefulWidget {
  const GlassButton({
    super.key,
    required this.label,
    required this.type,
    required this.palette,
    required this.onTap,
    this.active = false,
    this.fontSize,
    this.blur = false,
  });

  final String label;
  final ButtonType type;
  final GlassPalette palette;
  final VoidCallback onTap;

  /// Highlighted toggle (e.g. "Inv" while it is switched on).
  final bool active;
  final double? fontSize;

  /// Real BackdropFilter blur. Pretty, but slow on many phones.
  final bool blur;

  @override
  State<GlassButton> createState() => _GlassButtonState();
}

class _GlassButtonState extends State<GlassButton> {
  bool _pressed = false;
  Alignment _touch = Alignment.center;

  /// One calm, flat tint per key (Apple style: no gradients, no shine).
  Color _tint() {
    final p = widget.palette;
    if (widget.active) return p.accent.withValues(alpha: 0.85);
    switch (widget.type) {
      case ButtonType.top:
        return p.dark
            ? Colors.white.withValues(alpha: 0.30)
            : Colors.black.withValues(alpha: 0.12);
      case ButtonType.operator:
        return p.accent.withValues(alpha: 0.92);
      case ButtonType.equals:
        return GlassPalette.equalsGreen.withValues(alpha: 0.92);
      case ButtonType.scientific:
        return p.dark
            ? Colors.white.withValues(alpha: 0.10)
            : Colors.white.withValues(alpha: 0.6);
      case ButtonType.num:
        return p.dark
            ? Colors.white.withValues(alpha: 0.15)
            : Colors.white.withValues(alpha: 0.78);
    }
  }

  Color get _textColor {
    final colored = widget.active ||
        widget.type == ButtonType.operator ||
        widget.type == ButtonType.equals;
    return colored ? Colors.white : widget.palette.text;
  }

  @override
  Widget build(BuildContext context) {
    final p = widget.palette;
    final fill = Color.alphaBlend(
      Colors.white.withValues(alpha: _pressed ? 0.2 : 0.0),
      _tint(),
    );

    return RepaintBoundary(
      child: LayoutBuilder(
        builder: (context, c) {
          final w = c.maxWidth;
          final h = c.maxHeight;
          final d = math.min(w, h); // round key: diameter
          final base = widget.fontSize ??
              (widget.type == ButtonType.scientific ? 19.0 : 30.0);

          return GestureDetector(
            behavior: HitTestBehavior.opaque,
            onTapDown: (details) => setState(() {
              _pressed = true;
              _touch = Alignment(
                ((details.localPosition.dx - (w - d) / 2) / d * 2 - 1)
                    .clamp(-1.0, 1.0)
                    .toDouble(),
                ((details.localPosition.dy - (h - d) / 2) / d * 2 - 1)
                    .clamp(-1.0, 1.0)
                    .toDouble(),
              );
            }),
            onTapUp: (_) {
              setState(() => _pressed = false);
              widget.onTap();
            },
            onTapCancel: () => setState(() => _pressed = false),
            child: Center(
              child: SizedBox(
                width: d,
                height: d,
                // The "liquid" part: a fast squish in, a springy release.
                child: AnimatedScale(
                  scale: _pressed ? 0.9 : 1.0,
                  duration: Duration(milliseconds: _pressed ? 90 : 360),
                  curve: _pressed ? Curves.easeOutCubic : Curves.easeOutBack,
                  child: ClipOval(
                    child: _OptionalBlur(
                      blur: widget.blur,
                      sigma: 14,
                      child: Stack(
                        fit: StackFit.expand,
                        children: [
                          AnimatedContainer(
                            duration: const Duration(milliseconds: 140),
                            color: fill,
                          ),
                          // soft light that follows your finger
                          IgnorePointer(
                            child: AnimatedOpacity(
                              opacity: _pressed ? 1 : 0,
                              duration: Duration(milliseconds: _pressed ? 90 : 300),
                              child: DecoratedBox(
                                decoration: BoxDecoration(
                                  gradient: RadialGradient(
                                    center: _touch,
                                    radius: 0.85,
                                    colors: [
                                      Colors.white.withValues(alpha: 0.28),
                                      Colors.white.withValues(alpha: 0),
                                    ],
                                  ),
                                ),
                              ),
                            ),
                          ),
                          IgnorePointer(
                            child: CustomPaint(
                              painter: _RimPainter(radius: d / 2, dark: p.dark),
                            ),
                          ),
                          Center(
                            child: Padding(
                              padding: const EdgeInsets.symmetric(horizontal: 4),
                              child: FittedBox(
                                fit: BoxFit.scaleDown,
                                child: Text(
                                  widget.label,
                                  style: TextStyle(
                                    fontSize: math.min(base, d * 0.42),
                                    fontWeight: FontWeight.w500,
                                    color: _textColor,
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ],
                      ),
                    ),
                  ),
                ),
              ),
            ),
          );
        },
      ),
    );
  }
}

/// Thin edge that is bright at the top-left and bottom-right and clear in
/// between, like light catching the rim of a glass pebble.
class _RimPainter extends CustomPainter {
  _RimPainter({required this.radius, required this.dark});

  final double radius;
  final bool dark;

  @override
  void paint(Canvas canvas, Size size) {
    final rect = (Offset.zero & size).deflate(0.75);
    final rrect = RRect.fromRectAndRadius(rect, Radius.circular(radius));
    final k = dark ? 1.0 : 1.2;
    final paint = Paint()
      ..style = PaintingStyle.stroke
      ..strokeWidth = 1.0
      ..shader = LinearGradient(
        begin: Alignment.topLeft,
        end: Alignment.bottomRight,
        colors: [
          Colors.white.withValues(alpha: (0.5 * k).clamp(0.0, 1.0)),
          Colors.white.withValues(alpha: 0.06),
          Colors.white.withValues(alpha: 0.02),
          Colors.white.withValues(alpha: (0.2 * k).clamp(0.0, 1.0)),
        ],
        stops: const [0, 0.35, 0.65, 1],
      ).createShader(rect);
    canvas.drawRRect(rrect, paint);
  }

  @override
  bool shouldRepaint(_RimPainter old) =>
      old.radius != radius || old.dark != dark;
}

// ---------------------------------------------------------------------------
// Glass bottom sheet (history, settings).
// ---------------------------------------------------------------------------

/// Applies a backdrop blur only when [blur] is on (it is expensive).
class _OptionalBlur extends StatelessWidget {
  const _OptionalBlur({
    required this.blur,
    required this.sigma,
    required this.child,
  });

  final bool blur;
  final double sigma;
  final Widget child;

  @override
  Widget build(BuildContext context) {
    if (!blur) return child;
    return BackdropFilter(
      filter: ImageFilter.blur(sigmaX: sigma, sigmaY: sigma),
      child: child,
    );
  }
}

Future<T?> showGlassSheet<T>(BuildContext context, WidgetBuilder builder) {
  return showModalBottomSheet<T>(
    context: context,
    backgroundColor: Colors.transparent,
    elevation: 0,
    isScrollControlled: true,
    barrierColor: Colors.black.withValues(alpha: 0.4),
    builder: builder,
  );
}

class GlassSheet extends StatelessWidget {
  const GlassSheet({
    super.key,
    required this.palette,
    required this.child,
    this.heightFactor = 0.62,
    this.blur = false,
  });

  final GlassPalette palette;
  final Widget child;
  final double heightFactor;
  final bool blur;

  @override
  Widget build(BuildContext context) {
    final height = MediaQuery.of(context).size.height * heightFactor;
    return ClipRRect(
      borderRadius: const BorderRadius.vertical(top: Radius.circular(32)),
      child: _OptionalBlur(
        blur: blur,
        sigma: 28,
        child: Container(
          height: height,
          decoration: BoxDecoration(
            color: blur ? palette.sheetFill : palette.sheetFill.withValues(alpha: 0.92),
            border: Border(
              top: BorderSide(
                color: Colors.white.withValues(alpha: palette.dark ? 0.3 : 0.85),
                width: 1.2,
              ),
            ),
          ),
          child: SafeArea(
            top: false,
            child: Padding(
              padding: const EdgeInsets.fromLTRB(20, 16, 20, 8),
              child: child,
            ),
          ),
        ),
      ),
    );
  }
}

Future<void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  final settings = await AppSettings.load();
  final controller = CalcController(
    history: List.of(settings.history),
    onHistoryChanged: settings.saveHistory,
  );
  runApp(GlassCalcApp(settings: settings, controller: controller));
}

class GlassCalcApp extends StatelessWidget {
  const GlassCalcApp({
    super.key,
    required this.settings,
    required this.controller,
  });

  final AppSettings settings;
  final CalcController controller;

  @override
  Widget build(BuildContext context) {
    return ListenableBuilder(
      listenable: settings,
      builder: (context, _) {
        final accent = accentOptions[settings.accentIndex].color;
        final brightness = settings.isDark ? Brightness.dark : Brightness.light;
        return MaterialApp(
          debugShowCheckedModeBanner: false,
          title: 'Glass Calc',
          theme: ThemeData(
            useMaterial3: true,
            brightness: brightness,
            colorScheme: ColorScheme.fromSeed(
              seedColor: accent,
              brightness: brightness,
            ),
          ),
          home: CalculatorScreen(settings: settings, controller: controller),
        );
      },
    );
  }
}

class _Key {
  final String id;
  final String label;
  final ButtonType type;
  final bool active;
  final int flex;
  const _Key(this.id, this.label, this.type, {this.active = false, this.flex = 2});
}

class CalculatorScreen extends StatefulWidget {
  const CalculatorScreen({
    super.key,
    required this.settings,
    required this.controller,
  });

  final AppSettings settings;
  final CalcController controller;

  @override
  State<CalculatorScreen> createState() => _CalculatorScreenState();
}

class _CalculatorScreenState extends State<CalculatorScreen> {
  bool _scientificOpen = false;

  AppSettings get settings => widget.settings;
  CalcController get calc => widget.controller;

  GlassPalette get palette => GlassPalette(
        dark: settings.isDark,
        accent: accentOptions[settings.accentIndex].color,
      );

  final Uri _ytUrl =
      Uri.parse('https://www.youtube.com/channel/UCKhGVoXX01SXBxVp90-hGdg');

  Future<void> _openYouTubeChannel() async {
    try {
      if (!await launchUrl(_ytUrl, mode: LaunchMode.externalApplication)) {
        debugPrint('Could not launch $_ytUrl');
      }
    } catch (e) {
      debugPrint('Error launching URL: $e');
    }
  }

  void _press(String key) {
    if (settings.haptics) HapticFeedback.lightImpact();
    calc.handleKey(key);
  }

  void _copy() {
    final text = calc.copyText;
    if (text.isEmpty) return;
    Clipboard.setData(ClipboardData(text: text));
    if (settings.haptics) HapticFeedback.mediumImpact();
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(
        SnackBar(
          content: Text('Copied $text'),
          duration: const Duration(milliseconds: 1200),
          behavior: SnackBarBehavior.floating,
        ),
      );
  }

  // ------------------------------------------------------------------ build

  @override
  Widget build(BuildContext context) {
    return ListenableBuilder(
      listenable: Listenable.merge([settings, calc]),
      builder: (context, _) {
        final p = palette;
        final landscape =
            MediaQuery.of(context).orientation == Orientation.landscape;
        final overlay =
            (p.dark ? SystemUiOverlayStyle.light : SystemUiOverlayStyle.dark)
                .copyWith(
          statusBarColor: Colors.transparent,
          systemNavigationBarColor: Colors.transparent,
        );

        return AnnotatedRegion<SystemUiOverlayStyle>(
          value: overlay,
          child: Scaffold(
            backgroundColor: p.backgroundGradient.first,
            body: Stack(
              fit: StackFit.expand,
              children: [
                LiquidBackground(palette: p, animate: settings.liquidMotion),
                SafeArea(
                  child: landscape ? _landscape(p) : _portrait(p),
                ),
              ],
            ),
          ),
        );
      },
    );
  }

  Widget _portrait(GlassPalette p) {
    return Column(
      children: [
        _topBar(p),
        Expanded(flex: 2, child: _display(p, compact: false)),
        _scientificToggle(p),
        AnimatedContainer(
          duration: const Duration(milliseconds: 300),
          curve: Curves.fastOutSlowIn,
          height: _scientificOpen ? 190 : 0,
          padding: const EdgeInsets.symmetric(horizontal: 12),
          child: ClipRect(
            child: OverflowBox(
              maxHeight: 190,
              alignment: Alignment.topCenter,
              child: Column(
                children: [
                  for (final row in _sciRowsPortrait()) ...[
                    Expanded(child: _row(p, row, gap: 5)),
                    const SizedBox(height: 8),
                  ],
                ],
              ),
            ),
          ),
        ),
        Expanded(
          flex: 4,
          child: Padding(
            padding: const EdgeInsets.all(12),
            child: _grid(p, _standardRows(), gap: 10, hGap: 5),
          ),
        ),
      ],
    );
  }

  Widget _landscape(GlassPalette p) {
    return Column(
      children: [
        _topBar(p, compact: true),
        Expanded(flex: 3, child: _display(p, compact: true)),
        Expanded(
          flex: 8,
          child: Padding(
            padding: const EdgeInsets.fromLTRB(12, 0, 12, 8),
            child: Row(
              children: [
                Expanded(
                  flex: 3,
                  child: _grid(p, _sciRowsLandscape(), gap: 6, hGap: 4),
                ),
                const SizedBox(width: 14),
                Expanded(
                  flex: 4,
                  child: _grid(p, _standardRows(), gap: 6, hGap: 4),
                ),
              ],
            ),
          ),
        ),
      ],
    );
  }

  // ------------------------------------------------------------ key layouts

  List<List<_Key>> _standardRows() => const [
        [
          _Key('AC', 'AC', ButtonType.top),
          _Key('( )', '( )', ButtonType.top),
          _Key('%', '%', ButtonType.top),
          _Key('÷', '÷', ButtonType.operator),
        ],
        [
          _Key('7', '7', ButtonType.num),
          _Key('8', '8', ButtonType.num),
          _Key('9', '9', ButtonType.num),
          _Key('×', '×', ButtonType.operator),
        ],
        [
          _Key('4', '4', ButtonType.num),
          _Key('5', '5', ButtonType.num),
          _Key('6', '6', ButtonType.num),
          _Key('-', '−', ButtonType.operator),
        ],
        [
          _Key('1', '1', ButtonType.num),
          _Key('2', '2', ButtonType.num),
          _Key('3', '3', ButtonType.num),
          _Key('+', '+', ButtonType.operator),
        ],
        [
          _Key('0', '0', ButtonType.num),
          _Key('.', '.', ButtonType.num),
          _Key('⌫', '⌫', ButtonType.num),
          _Key('=', '=', ButtonType.equals),
        ],
      ];

  _Key _sin(String id) => _Key(
      id, calc.inverse ? '$id⁻¹' : id, ButtonType.scientific);

  _Key get _sqrtKey =>
      _Key('sqrt', calc.inverse ? 'x²' : '√', ButtonType.scientific);
  _Key get _radKey =>
      _Key('rad', calc.isRad ? 'Rad' : 'Deg', ButtonType.scientific);
  _Key get _invKey =>
      _Key('inv', 'Inv', ButtonType.scientific, active: calc.inverse);
  _Key get _lnKey =>
      _Key('ln', calc.inverse ? 'eˣ' : 'ln', ButtonType.scientific);
  _Key get _logKey =>
      _Key('log', calc.inverse ? '10ˣ' : 'log', ButtonType.scientific);

  List<List<_Key>> _sciRowsPortrait() => [
        [
          _sqrtKey,
          const _Key('π', 'π', ButtonType.scientific),
          const _Key('^', '^', ButtonType.scientific),
          const _Key('!', '!', ButtonType.scientific),
        ],
        [_radKey, _sin('sin'), _sin('cos'), _sin('tan')],
        [
          _invKey,
          const _Key('e', 'e', ButtonType.scientific),
          _lnKey,
          _logKey,
        ],
      ];

  List<List<_Key>> _sciRowsLandscape() => [
        [_invKey, _radKey, const _Key('!', '!', ButtonType.scientific)],
        [_sin('sin'), _sin('cos'), _sin('tan')],
        [
          _lnKey,
          _logKey,
          const _Key('e', 'e', ButtonType.scientific),
        ],
        [
          const _Key('π', 'π', ButtonType.scientific),
          _sqrtKey,
          const _Key('^', '^', ButtonType.scientific),
        ],
        const [
          _Key('sq', 'x²', ButtonType.scientific, flex: 3),
          _Key('recip', '1/x', ButtonType.scientific, flex: 3),
        ],
      ];

  /// Rows of keys. A row's keys share its width by their `flex`.
  Widget _grid(GlassPalette p, List<List<_Key>> rows,
      {required double gap, required double hGap}) {
    return Column(
      children: [
        for (var i = 0; i < rows.length; i++) ...[
          Expanded(child: _row(p, rows[i], gap: hGap)),
          if (i != rows.length - 1) SizedBox(height: gap),
        ],
      ],
    );
  }

  Widget _row(GlassPalette p, List<_Key> keys, {required double gap}) {
    return Row(
      children: [
        for (final k in keys)
          Expanded(
            flex: k.flex,
            child: Padding(
              padding: EdgeInsets.symmetric(horizontal: gap),
              child: GlassButton(
                label: k.label,
                type: k.type,
                palette: p,
                active: k.active,
                blur: settings.realBlur,
                onTap: () => _press(k.id),
              ),
            ),
          ),
      ],
    );
  }

  // ----------------------------------------------------------------- pieces

  Widget _topBar(GlassPalette p, {bool compact = false}) {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 16, vertical: compact ? 0 : 8),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          IconButton(
            tooltip: 'History',
            icon: Icon(Icons.history, color: p.textDim, size: compact ? 24 : 28),
            onPressed: _showHistorySheet,
          ),
          Builder(
            builder: (btnContext) => IconButton(
              tooltip: 'Menu',
              icon: Icon(Icons.more_vert, color: p.textDim, size: compact ? 24 : 28),
              onPressed: _showMenu,
            ),
          ),
        ],
      ),
    );
  }

  Widget _display(GlassPalette p, {required bool compact}) {
    final len = calc.input.length;
    double size;
    if (calc.isEvaluated) {
      size = len > 10 ? 44 : 68;
    } else {
      size = len > 14 ? 36 : (len > 9 ? 44 : 52);
    }
    if (compact) size = size * 0.7;

    return LayoutBuilder(builder: (context, cons) {
      // Shrink the big number if needed so it always fits (no overflow).
      final reserved = (compact ? 4.0 : 24.0) + (compact ? 24.0 : 46.0);
      final maxSize = (cons.maxHeight - reserved) / 1.25;
      final fontSize = size.clamp(18.0, maxSize < 18.0 ? 18.0 : maxSize).toDouble();
      return GestureDetector(
      behavior: HitTestBehavior.opaque,
      onLongPress: _copy,
      child: Container(
        padding: EdgeInsets.symmetric(horizontal: 24, vertical: compact ? 2 : 12),
        alignment: Alignment.bottomRight,
        child: Column(
          mainAxisAlignment: MainAxisAlignment.end,
          crossAxisAlignment: CrossAxisAlignment.end,
          children: [
            SingleChildScrollView(
              scrollDirection: Axis.horizontal,
              reverse: true,
              child: Text(
                calc.input.isEmpty ? '0' : calc.input,
                style: TextStyle(
                  fontSize: fontSize,
                  fontWeight: calc.isEvaluated ? FontWeight.bold : FontWeight.w400,
                  color: p.text,
                  letterSpacing: -0.5,
                ),
              ),
            ),
            SizedBox(height: compact ? 0 : 6),
            if (calc.errorText.isNotEmpty)
              Text(
                calc.errorText,
                style: TextStyle(
                  fontSize: compact ? 15 : 20,
                  fontWeight: FontWeight.w500,
                  color: p.error,
                ),
              )
            else
              AnimatedOpacity(
                duration: const Duration(milliseconds: 150),
                opacity: calc.preview.isNotEmpty ? 1.0 : 0.0,
                child: Text(
                  calc.preview.isEmpty ? ' ' : calc.preview,
                  style: TextStyle(
                    fontSize: compact ? 20 : 32,
                    fontWeight: FontWeight.w300,
                    color: p.preview,
                    letterSpacing: -0.5,
                  ),
                ),
              ),
          ],
        ),
      ),
    );
    });
  }

  Widget _scientificToggle(GlassPalette p) {
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 4),
      child: Row(
        children: [
          GestureDetector(
            onTap: () => setState(() => _scientificOpen = !_scientificOpen),
            child: AnimatedRotation(
              turns: _scientificOpen ? 0.5 : 0.0,
              duration: const Duration(milliseconds: 300),
              child: Container(
                padding: const EdgeInsets.all(6),
                decoration: BoxDecoration(
                  color: p.dark
                      ? Colors.white.withValues(alpha: 0.12)
                      : Colors.black.withValues(alpha: 0.08),
                  shape: BoxShape.circle,
                ),
                child: Icon(Icons.keyboard_arrow_down, color: p.textDim, size: 22),
              ),
            ),
          ),
        ],
      ),
    );
  }

  // ------------------------------------------------------------------- menu

  void _showMenu() {
    final p = palette;
    showGeneralDialog<void>(
      context: context,
      barrierDismissible: true,
      barrierLabel: 'Close menu',
      barrierColor: Colors.transparent,
      transitionDuration: const Duration(milliseconds: 240),
      pageBuilder: (dialogContext, a1, a2) {
        final top = MediaQuery.of(dialogContext).padding.top + 52;
        return Align(
          alignment: Alignment.topRight,
          child: Padding(
            padding: EdgeInsets.only(top: top, right: 14),
            child: _menuCard(p, dialogContext),
          ),
        );
      },
      // Grows out of the ⋮ button with a soft spring.
      transitionBuilder: (context, anim, secondary, child) {
        final curved = CurvedAnimation(
          parent: anim,
          curve: Curves.easeOutBack,
          reverseCurve: Curves.easeInCubic,
        );
        return FadeTransition(
          opacity: anim,
          child: ScaleTransition(
            alignment: Alignment.topRight,
            scale: Tween<double>(begin: 0.8, end: 1.0).animate(curved),
            child: child,
          ),
        );
      },
    );
  }

  Widget _menuCard(GlassPalette p, BuildContext dialogContext) {
    Widget item(IconData icon, String label, VoidCallback onTap) {
      return InkWell(
        borderRadius: BorderRadius.circular(16),
        onTap: () {
          Navigator.pop(dialogContext);
          onTap();
        },
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 14, vertical: 12),
          child: Row(
            children: [
              Icon(icon, size: 20, color: p.text),
              const SizedBox(width: 12),
              Text(
                label,
                style: TextStyle(
                  color: p.text,
                  fontSize: 15,
                  fontWeight: FontWeight.w500,
                ),
              ),
            ],
          ),
        ),
      );
    }

    return Material(
      color: Colors.transparent,
      child: DecoratedBox(
        decoration: BoxDecoration(
          borderRadius: BorderRadius.circular(24),
          boxShadow: [
            BoxShadow(
              color: Colors.black.withValues(alpha: p.dark ? 0.35 : 0.15),
              blurRadius: 28,
              offset: const Offset(0, 10),
            ),
          ],
        ),
        child: ClipRRect(
          borderRadius: BorderRadius.circular(24),
          child: _OptionalBlur(
            blur: settings.realBlur,
            sigma: 24,
            child: Container(
              width: 240,
              padding: const EdgeInsets.all(6),
              decoration: BoxDecoration(
                color: p.menuColor.withValues(alpha: settings.realBlur ? 0.7 : 0.96),
                borderRadius: BorderRadius.circular(24),
                border: Border.all(
                  color: Colors.white.withValues(alpha: p.dark ? 0.12 : 0.7),
                ),
              ),
              child: Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  item(Icons.tune_rounded, 'Settings', _showSettingsSheet),
                  Divider(
                    height: 10,
                    thickness: 1,
                    indent: 14,
                    endIndent: 14,
                    color: p.textDim.withValues(alpha: 0.18),
                  ),
                  InkWell(
                    borderRadius: BorderRadius.circular(16),
                    onTap: () {
                      Navigator.pop(dialogContext);
                      _openYouTubeChannel();
                    },
                    child: Padding(
                      padding: const EdgeInsets.symmetric(horizontal: 14, vertical: 10),
                      child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Text(
                            'Made by Raihan ⚡',
                            style: TextStyle(color: p.textDim, fontSize: 12.5),
                          ),
                          Row(
                            mainAxisSize: MainAxisSize.min,
                            children: [
                              const Icon(Icons.play_circle_fill,
                                  size: 17, color: Color(0xFFFF3B30)),
                              const SizedBox(width: 5),
                              Text(
                                'YouTube',
                                style: TextStyle(color: p.textDim, fontSize: 12.5),
                              ),
                            ],
                          ),
                        ],
                      ),
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }

  // ----------------------------------------------------------------- sheets

  void _showHistorySheet() {
    showGlassSheet<void>(context, (sheetContext) {
      return ListenableBuilder(
        listenable: Listenable.merge([settings, calc]),
        builder: (context, _) {
          final p = palette;
          return GlassSheet(
            palette: p,
            blur: settings.realBlur,
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Row(
                  children: [
                    Expanded(
                      child: Text(
                        'History',
                        style: TextStyle(
                          fontSize: 22,
                          fontWeight: FontWeight.bold,
                          color: p.text,
                        ),
                      ),
                    ),
                    if (calc.history.isNotEmpty)
                      TextButton(
                        onPressed: calc.clearHistory,
                        child: const Text('Clear'),
                      ),
                    IconButton(
                      icon: Icon(Icons.close, color: p.textDim),
                      onPressed: () => Navigator.pop(sheetContext),
                    ),
                  ],
                ),
                if (calc.history.isNotEmpty)
                  Text(
                    'Tap to use the answer · Hold to use the sum',
                    style: TextStyle(color: p.textDim, fontSize: 13),
                  ),
                Divider(color: p.textDim.withValues(alpha: 0.3)),
                Expanded(
                  child: calc.history.isEmpty
                      ? Center(
                          child: Text(
                            'No history recorded yet.',
                            style: TextStyle(color: p.textDim, fontSize: 16),
                          ),
                        )
                      : ListView.separated(
                          itemCount: calc.history.length,
                          separatorBuilder: (context, i) =>
                              Divider(color: p.textDim.withValues(alpha: 0.15)),
                          itemBuilder: (context, i) {
                            final entry = calc.history[i];
                            return InkWell(
                              borderRadius: BorderRadius.circular(12),
                              onTap: () {
                                calc.useHistory(entry);
                                Navigator.pop(sheetContext);
                              },
                              onLongPress: () {
                                calc.useHistory(entry, useExpression: true);
                                Navigator.pop(sheetContext);
                              },
                              child: Padding(
                                padding: const EdgeInsets.symmetric(
                                    vertical: 6, horizontal: 8),
                                child: Column(
                                  crossAxisAlignment: CrossAxisAlignment.end,
                                  children: [
                                    SizedBox(
                                      width: double.infinity,
                                      child: Text(
                                        entry.expression,
                                        textAlign: TextAlign.right,
                                        style: TextStyle(
                                          color: p.textDim,
                                          fontSize: 18,
                                        ),
                                      ),
                                    ),
                                    const SizedBox(height: 4),
                                    SizedBox(
                                      width: double.infinity,
                                      child: Text(
                                        '= ${entry.result}',
                                        textAlign: TextAlign.right,
                                        style: TextStyle(
                                          color: p.preview,
                                          fontSize: 26,
                                          fontWeight: FontWeight.bold,
                                        ),
                                      ),
                                    ),
                                  ],
                                ),
                              ),
                            );
                          },
                        ),
                ),
              ],
            ),
          );
        },
      );
    });
  }

  void _showSettingsSheet() {
    showGlassSheet<void>(context, (sheetContext) {
      return ListenableBuilder(
        listenable: settings,
        builder: (context, _) {
          final p = palette;
          TextStyle title = TextStyle(color: p.text, fontSize: 16);
          return GlassSheet(
            palette: p,
            blur: settings.realBlur,
            heightFactor: 0.6,
            child: ListView(
              padding: EdgeInsets.zero,
              children: [
                Row(
                  children: [
                    Expanded(
                      child: Text(
                        'Settings',
                        style: TextStyle(
                          fontSize: 22,
                          fontWeight: FontWeight.bold,
                          color: p.text,
                        ),
                      ),
                    ),
                    IconButton(
                      icon: Icon(Icons.close, color: p.textDim),
                      onPressed: () => Navigator.pop(sheetContext),
                    ),
                  ],
                ),
                SwitchListTile(
                  contentPadding: EdgeInsets.zero,
                  title: Text('Dark theme', style: title),
                  value: settings.isDark,
                  onChanged: settings.setDark,
                ),
                SwitchListTile(
                  contentPadding: EdgeInsets.zero,
                  title: Text('Vibration on tap', style: title),
                  value: settings.haptics,
                  onChanged: settings.setHaptics,
                ),
                SwitchListTile(
                  contentPadding: EdgeInsets.zero,
                  title: Text('Liquid motion', style: title),
                  subtitle: Text(
                    'Moving background. Turn off to save battery.',
                    style: TextStyle(color: p.textDim, fontSize: 12),
                  ),
                  value: settings.liquidMotion,
                  onChanged: settings.setLiquidMotion,
                ),
                SwitchListTile(
                  contentPadding: EdgeInsets.zero,
                  title: Text('Real glass blur', style: title),
                  subtitle: Text(
                    'Prettier but slower. Leave off on older phones.',
                    style: TextStyle(color: p.textDim, fontSize: 12),
                  ),
                  value: settings.realBlur,
                  onChanged: settings.setRealBlur,
                ),
                const SizedBox(height: 12),
                Text('Accent colour', style: title),
                const SizedBox(height: 12),
                Wrap(
                  spacing: 14,
                  children: [
                    for (var i = 0; i < accentOptions.length; i++)
                      Semantics(
                        label: accentOptions[i].name,
                        button: true,
                        child: GestureDetector(
                          onTap: () => settings.setAccent(i),
                          child: Container(
                            width: 44,
                            height: 44,
                            decoration: BoxDecoration(
                              color: accentOptions[i].color,
                              shape: BoxShape.circle,
                              border: Border.all(
                                color: settings.accentIndex == i
                                    ? p.text
                                    : Colors.white.withValues(alpha: 0.3),
                                width: settings.accentIndex == i ? 3 : 1.5,
                              ),
                            ),
                            child: settings.accentIndex == i
                                ? const Icon(Icons.check, color: Colors.white)
                                : null,
                          ),
                        ),
                      ),
                  ],
                ),
              ],
            ),
          );
        },
      );
    });
  }
}

