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
 * The annotated target is checked to be a valid SSN.
 * A valid SSN follows Swedish SSN standard and defines by YYYYMMDD-XXXX
 * 
 * @author Konstantin Sozinov
 */
@Constraint(validatedBy = ValidSSN.SSNValidator.class)
@Documented
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidSSN {

    String message() default "{invalidSSN}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    class SSNValidator implements ConstraintValidator<ValidSSN, String> {

        @Override   
        public void initialize(ValidSSN constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            if (isEmpty(value, context)) {
                return false;
            }
            if(!(value.matches("^[12]{1}[90]{1}[0-9]{6}-[0-9]{4}$"))) return false;
           return true;
           
        }

        private boolean isEmpty(String value, ConstraintValidatorContext context) {
            if (value.length() == 0) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("{emptySSN}").addConstraintViolation();
                return true;
            }
            return false;
        }
    }
}
