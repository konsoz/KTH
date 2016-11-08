package se.kth.ict.IV1201.project.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import javax.validation.Payload;

/**
 * The annotated target is checked to be a valid email.
 * A valid email follows name@domain.countrycode
 * 
 * @author Konstantin Sozinov
 */
@Constraint(validatedBy = ValidEmail.EmailValidator.class)
@Documented
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidEmail {

    String message() default "{invalidEmail}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    class EmailValidator implements ConstraintValidator<ValidEmail, String> {

        @Override   
        public void initialize(ValidEmail constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            if (isEmpty(value, context)) {
                return false;
            }
            if(!(value.matches("^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"))) return false;
           return true;
           
        }

        private boolean isEmpty(String value, ConstraintValidatorContext context) {
            if (value.length() == 0) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("{emptyEmail}").addConstraintViolation();
                return true;
            }
            return false;
        }
    }
}
