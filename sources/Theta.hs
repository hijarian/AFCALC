module Theta where
-- Реализация тета-функций на основе тригонометрических рядов.
-- Тета-функции зависят от параметра tau, мнимая часть которого должна быть положительна.
-- Тау используется только в параметре 'q' в самих функциях, так что можно считать, что математически тета-функции зависят от кью, но технически работа этого модуля зависит от наличия тау.
-- Любую функцию надо вызывать через thetaN <n> (qrap <tau>) <u>

import Control.Parallel
import Control.Parallel.Strategies

-- Мы работаем с комплексными числами
import Data.Complex

-- Параметр q из математического представления тета-функций.
qpar tau = exp $ pi * tau * (0 :+ 1)

-- Функция, изображающая из себя (-1)^n
signfun :: (RealFloat a) => Integer -> Complex a
signfun nn
  | odd nn = -1
  | otherwise = 1

-- Функция \Theta_1
theta1 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta1 n q u = 2 * sum thetaarg
   where thetaarg = [(signfun nn) * (qfun q nn) * (sinfun u nn) | nn <- [1..n]]
         qfun :: (RealFloat a) => Complex a -> Integer -> Complex a
         qfun q nn = q ** (0.5 + fromInteger nn) ** 2
         sinfun :: (RealFloat a) => Complex a -> Integer -> Complex a
         sinfun u nn = sin $ fromInteger(2 * nn + 1) * u

-- Функция \Theta_2
theta2 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta2 n q u = 2 * sum thetaarg
  where thetaarg = [(qfun q nn) * (cosfun u nn) | nn <- [1..n]]
        qfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        qfun q nn = q ** (0.5 + fromInteger nn) ** 2
        cosfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        cosfun u nn = cos $ fromInteger(2 * nn + 1)  * u

-- Функция \Theta_3
theta3 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta3 n q u = 1 + 2 * sum thetaarg
  where thetaarg = [(qfun q nn) * (cosfun u nn) | nn <- [1..n]]
        qfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        qfun q nn = q ** (fromInteger nn) ** 2
        cosfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        cosfun u nn = cos $ fromInteger (2 * nn) * u

-- Функция \Theta_4
theta4 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta4 n q u = 1 + 2 * sum thetaarg
  where thetaarg = [(signfun nn) * (qfun q nn) * (cosfun u nn) | nn <- [1..n]]
        qfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        qfun q nn = q ** fromInteger(nn) ** 2
        cosfun :: (RealFloat a) => Complex a -> Integer -> Complex a
        cosfun u nn = cos $ fromInteger(2 * nn)  * u

