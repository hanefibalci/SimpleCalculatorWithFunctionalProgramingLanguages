# Calculator Implementations

Bu repoda farklı programlama dillerinde geliştirilmiş hesap makinesi uygulamaları bulunmaktadır. Aşağıda, her bir dosyanın kısa açıklaması ve nasıl çalıştırılacağı yer almaktadır.

## İçerik

- **calculator.adb** – Ada dilinde yazılmış hesap makinesi uygulaması.
- **calculator.pl** – Perl dilinde, değişken atama ve aritmetik ifadelerin hesaplanmasını destekleyen hesap makinesi.
- **calculator.rkt** – Schema dilinin bir alt sınıfı olan racket dilinde, infix ifadeleri prefix ifadeye çevirip hesaplama yapan uygulama.
- **calculator.rs** – Rust dilinde, ifadelerin değerlendirilmesi ve değişken atamaları destekleyen hesap makinesi.
- **calculator_.pl** – Prolog dilinde, DCG kullanarak matematiksel ifadeleri ayrıştıran ve hesaplayan uygulama.

---

## Ada Uygulaması (calculator.adb)

**Açıklama:**  
Bu Ada uygulaması, kullanıcıya toplama, çıkarma, çarpma ve bölme işlemleri yapma imkanı tanır. Ayrıca, çoklu tamsayıların toplanması gibi işlemleri de destekler.

**Çalıştırma Adımları:**

1. GNAT gibi bir Ada derleyicisi kurulu olduğundan emin olun.
2. Terminalde, uygulamanın bulunduğu dizine gidin ve şu komutları çalıştırın:

   ```bash
   gnatmake calculator.adb
   ./calculator
